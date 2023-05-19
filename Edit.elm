module Main exposing (..)

import Browser
import Bytes
import Bytes.Decode as Bytes
import Dict
import Hex.Convert
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode as Json
import Json.Decode.Extra
import Maybe exposing (Maybe)
import Maybe.Extra
import ProOperate
import ProOperate.Card as Card
import ProOperate.Config as Config exposing (Config_pro2, defaultConfig_pro2)
import ProOperate.Touch as Touch exposing (TouchResponse)
import Procedure
import Procedure.Program
import Result.Extra
import Task exposing (Task)
import Task.Extra
import Time
import Time.Format as TimeFormat
import Time.Format.Config.Config_ja_jp as TimeFormatConfig
import TimeZone
import WebSQL

-- 練馬キッズの仕様は・・・ 
-- １．入退室カウント（IDMユニーク）
-- ２．カウントの表示位置は左上と右上
-- ３．深夜０：００にリセット
-- ってのが見た目の仕様で、裏方では
-- ４．タッチされたらAPIコール（親御さんにメールがいく）
-- ５．ハートビートと言われる定期APIコール（死活監視）
-- ６．ネットワークにつながらない場合のタッチの再送（４を再度行う）
-- ７．ネットワークエラーを認識してネットワークエラーがわかるようにする（以前はネットワークエラー画面表示。現状は仕様ドロップ）
-- PitTouch -- 10秒以内に再タッチされた場合は退室としてカウントしない（一番最初の仕様）

type Error
    = HttpError Http.Error
    | DecodeError String
    | ProOperateError ProOperate.Error
    | WebSQLError WebSQL.Error


type alias Setting =
    { name : String
    }


type alias Touch =
    { id : Int
    , idm : String
    , zoneName : String
    , data : Maybe String
    , createdAt : Int
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
    | OnTouch TouchResponse
    | OnTouchWithTime TouchResponse (Maybe String) Time.Posix



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , dbh : Maybe String
    , touches : List Touch
    , lastTouch : Maybe Touch
    }
-- lastTouchいらない

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
        |> Task.map (Json.decodeString settingDecoder)
        |> Task.map (Result.mapError Json.errorToString)
        |> Task.andThen
            (Result.Extra.unpack (DecodeError >> Task.fail) Task.succeed)


settingDecoder : Json.Decoder Setting
settingDecoder =
    let
        decoder =
            Json.map Setting
                (Json.field "name" Json.string)
    in
    Json.field "settings" decoder


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
-- 削除、指定したIDMの全件取得関数に変更
    let
        andMap =
            Json.Decode.Extra.andMap

        sql =
            """
            SELECT * FROM TOUCH ORDER BY CREATED DESC;
            """

        touchDecoder =
            Json.succeed Touch
                |> andMap (Json.field "ID" Json.int)
                |> andMap (Json.field "IDM" Json.string)
                |> andMap (Json.field "ZONENAME" Json.string)
                |> andMap (Json.field "DATA" (Json.maybe Json.string))
                |> andMap (Json.field "CREATED" Json.int)

        decoder =
            Json.list touchDecoder
    in
    WebSQL.querySQL decoder sql [] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLAllTouch)


selectCurrentTimeParamResultCmd : String -> Int 
selectCurrentTimeParamResultCmd idm millis = 
-- タッチした時刻から10秒以内のレコードがあるかどうかの関数 → ない場合は空のリストで返ってくる
    let
        sql =
            """
            SELECT ID, IDM, ZONENAME, DATA, CREATED      
            FROM TOUCH WHERE IDM = ? AND (CREATED + (10 * 1000)) > ?
            ORDER BY CREATED DESC
            LIMIT 1;
            """
    in
    WebSQL.querySQL decoder sql [ idm, millis ] dbh
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

            -- 10秒以内あるかどうか分岐 touch zoneName posix ->
            -- IDMとPOSIX使ってDB SELECT
            -- (model | posix = posix, zone = zoneName)
            """
            SELECT ID, IDM, ZONENAME, DATA, CREATED      
            FROM TOUCH WHERE IDM = "010101124e14d228" AND (CREATED + (10 * 1000)) > 1671684015469
            ORDER BY CREATED DESC
            LIMIT 1;
            """
            -- touch zoneName posix

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
            -- Maybe.unpack (問い合わせ結果) (Insert List Touch)

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
         -- SQLAllTouch なくしてよし、DBに問い合わせてカウント計算したものをここで上書き
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
            """
            selectIdmParamResultCmd

            SELECT ID, IDM, ZONENAME, DATA, CREATED, 
            COUNT(IDM) as COUNT,
            (COUNT(IDM) + 1) / 2 AS INCOUNT,
            FLOOR(COUNT(IDM) / 2) AS OUTCOUNT
            FROM TOUCH;
            """



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
    Just TimeFormat.format
        |> Maybe.Extra.andMap (Just TimeFormatConfig.config)
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
                |> Maybe.andThen (Bytes.decode (Bytes.unsignedInt16 Bytes.LE))
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