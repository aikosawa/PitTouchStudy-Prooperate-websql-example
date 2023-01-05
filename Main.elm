module Main exposing (..)

import Browser
import Html exposing (Html, div, p, text, ul, li)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode
import Json.Decode.Extra
import Hex
import Hex.Convert
import Bytes
import Bytes.Decode
import ProOperate
import ProOperate.Card as Card
import ProOperate.Config as Config exposing (Config_pro2, defaultConfig_pro2)
import ProOperate.Touch as Touch exposing (TouchResponse)
import Procedure
import Procedure.Program
import Maybe exposing (Maybe)
import Maybe.Extra
import Result.Extra
import Task exposing (Task)
import Task.Extra
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Time
import Time.Format
import Time.Format.Config.Config_ja_jp
import TimeZone
import WebSQL


type Error
    = HttpError Http.Error
    | DecodeError String
    | ProOperateError ProOperate.Error
    | WebSQLError WebSQL.Error


type alias Setting =
    { name : String
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ProcMsg (Procedure.Program.Msg Msg)
    | GotSetting Setting
    | GotDBHandle String
    | SQLDone String
    | SQLAllTouch (List Touch)
    | OnError Error
    | OnTouch TouchResponse -- Time.Posix, Time.Zone の変換用分岐
    | OnTouchWithTime TouchResponse (Maybe String) Time.Posix



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , dbh : Maybe String
    , touches : List Touch
    , lastTouch : Maybe Touch
    }


type alias Touch =
    { id : Int
    , idm : String
    , zoneName : String
    , data : Maybe String
    , createdAt : Int
    }

userTouch =
    { id = 0
    , idm = ""
    , zoneName = ""
    , millis = 0
    }

defaultTouch =
    { data = Nothing
    , idm = ""
    , zoneName = ""
    , millis = 0
    }


type alias Flags =
    {}



-- FUNCTIONS


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { procModel = Procedure.Program.init
      , config = defaultConfig_pro2
      , dbh = Nothing
      , touches = []
      , lastTouch = Nothing
      }
    , getProviderSettingCmd
    )


getProviderSetting : Task Error Setting
getProviderSetting =
    Http.Tasks.get
        { url = "http://localhost/providersetting.json"
        , resolver = Http.Tasks.resolveString
        }
        |> Task.mapError HttpError
        |> Task.map (Json.Decode.decodeString settingDecoder)
        |> Task.map (Result.mapError Json.Decode.errorToString)
        |> Task.andThen
            (Result.Extra.unpack (DecodeError >> Task.fail) Task.succeed)


settingDecoder : Json.Decode.Decoder Setting
settingDecoder =
    let
        decoder =
            Json.Decode.map Setting
                (Json.Decode.field "name" Json.Decode.string)
    in
    Json.Decode.field "settings" decoder


getProviderSettingCmd : Cmd Msg
getProviderSettingCmd =
    let
        toMsg =
            Result.Extra.unpack OnError GotSetting
    in
    getProviderSetting
        |> Task.attempt toMsg


configFromSetting : Setting -> Config_pro2
configFromSetting setting =
    defaultConfig_pro2
        |> (\r -> { r | felicaList = [ Card.sapica, Card.suica, Card.felica ] })
        |> (\r -> { r | waitLamp = "WW1L" })


{-| -}
observeTouchCmd : Config_pro2 -> Cmd Msg
observeTouchCmd config =
    let
        andMap =
            Procedure.map2 (|>)

        getZoneName =
            TimeZone.getZone
                |> Task.map (Just << Tuple.first)
                |> Task.onError ((always << Task.succeed) Nothing)
    in
    Procedure.map OnTouch (Touch.observeOnce_pro2 config)
        |> Procedure.try ProcMsg
            (Result.Extra.extract (OnError << ProOperateError))


createTableCmd : Cmd Msg
createTableCmd =
    let
        name =
            "example database"

        version =
            "1.0.0"

        desc =
            "database for example"

        -- 5MB
        size =
            5 * 1024 * 1024

        createTableSQL =
            """
            CREATE TABLE IF NOT EXISTS TOUCH (
              ID       INTEGER PRIMARY KEY AUTOINCREMENT,
              IDM      VARCHAR(32) NOT NULL,
              ZONENAME VARCHAR(32) NOT NULL,
              DATA     TEXT,
              CREATED  INTEGER NOT NULL
            );
            """
    in
    WebSQL.openDatabase name version desc size
        |> Procedure.andThen (WebSQL.runSQL createTableSQL [])
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) GotDBHandle)


insertTouchCmd :
    String
    -> String
    -> String
    -> Time.Posix
    -> String
    -> Cmd Msg
insertTouchCmd idm data zoneName posix dbh =
    let
        sql =
            """
            INSERT INTO TOUCH (
              IDM, DATA, ZONENAME, CREATED
            ) 
            VALUES (?, ?, ?, ?);
            """

        createdAt =
            Time.posixToMillis posix
                |> String.fromInt
    in
    WebSQL.runSQL sql [ idm, data, zoneName, createdAt ] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLDone)


dropTouchTableCmd : String -> Cmd Msg
dropTouchTableCmd dbh =
    let
        sql =
            """
            DROP TABLE IF EXISTS TOUCH;
            """
    in
    WebSQL.runSQL sql [] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLDone)


selectAllTouchCmd : String -> Cmd Msg
selectAllTouchCmd dbh =
    let
        andMap =
            Json.Decode.Extra.andMap

        sql =
            """
            SELECT * FROM TOUCH ORDER BY CREATED DESC;
            """

        touchDecoder =
            Json.Decode.succeed Touch
                |> andMap (Json.Decode.field "ID" Json.Decode.int)
                |> andMap (Json.Decode.field "IDM" Json.Decode.string)
                |> andMap (Json.Decode.field "ZONENAME" Json.Decode.string)
                |> andMap (Json.Decode.field "DATA" (Json.Decode.maybe Json.Decode.string))
                |> andMap (Json.Decode.field "CREATED" Json.Decode.int)

        decoder =
            Json.Decode.list touchDecoder
    in
    WebSQL.querySQL decoder sql [] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLAllTouch)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            case msg of
                ProcMsg _ ->
                    msg

                _ ->
                    Debug.log "update:" msg
    in
    case msg of
        ProcMsg pMsg ->
            Procedure.Program.update pMsg model.procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        OnError err ->
            ( model
            , Cmd.none
            )

        OnTouch touch ->
            let
                andMap =
                    Task.Extra.andMap

                getZoneName =
                    TimeZone.getZone
                        |> Task.map (Just << Tuple.first)
                        |> Task.onError ((always << Task.succeed) Nothing)
            in
            ( model
            , Task.succeed (OnTouchWithTime touch)
                |> andMap getZoneName
                |> andMap Time.now
                |> Task.perform identity
            )

        OnTouchWithTime touch zoneName posix ->
            ( model
            , Just insertTouchCmd
                |> Maybe.Extra.andMap touch.idm
                |> Maybe.Extra.andMap (Maybe.Extra.orElse (Just "") touch.data)
                |> Maybe.Extra.andMap zoneName
                |> Maybe.Extra.andMap (Just posix)
                |> Maybe.Extra.andMap model.dbh
                |> Maybe.withDefault Cmd.none
            )

        GotSetting setting ->
            let
                config =
                    configFromSetting setting
            in
            ( { model | config = config }
            , createTableCmd
            )

        GotDBHandle dbh ->
            ( { model | dbh = Just dbh }
            , Task.succeed dbh
                |> Task.perform SQLDone
            )

        SQLAllTouch touches ->
            ( { model
                | touches = touches
                , lastTouch = List.head touches
              }
            , Cmd.none
            )

        SQLDone dbh ->
            ( model
            , Cmd.batch
                [ observeTouchCmd model.config
                , Maybe.map selectAllTouchCmd model.dbh
                    |> Maybe.withDefault Cmd.none
                ]
            )



-- VIEW


formatTime : Maybe Touch -> String
formatTime lastTouch =
    let
        nameToZone zoneName =
            Dict.get zoneName TimeZone.zones

        touchToZone touch =
            Maybe.map .zoneName lastTouch
                |> Maybe.andThen nameToZone
                |> Maybe.map (\zone -> zone ())

        touchToPosix touch =
            Maybe.map .createdAt lastTouch
                |> Maybe.map Time.millisToPosix
    in
    Just Time.Format.format
        |> Maybe.Extra.andMap (Just Time.Format.Config.Config_ja_jp.config)
        |> Maybe.Extra.andMap (Just "%Y-%m-%d %H:%M:%S")
        |> Maybe.Extra.andMap (touchToZone lastTouch)
        |> Maybe.Extra.andMap (touchToPosix lastTouch)
        |> Maybe.withDefault "--/--/-- 00:00:00"


view : Model -> Html Msg
view model =
    let
        idmText =
            model.lastTouch
                |> Maybe.map .idm
                |> Maybe.withDefault ""

        deposit =
            model.lastTouch
                |> Maybe.andThen .data
                |> Maybe.map (String.slice 20 24)
                |> Maybe.andThen Hex.Convert.toBytes
                |> Maybe.andThen (Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE))
                |> Maybe.withDefault 0
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ text <| formatTime model.lastTouch ] ]
        , div [] [ p [] [ text idmText ] ]
        , div [] [ p [] [ text <| String.fromInt deposit ] ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Procedure.Program.subscriptions model.procModel
        ]