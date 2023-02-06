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
    | SQLAllTouch (List TouchData)
    | OnError Error
    | OnTouch TouchResponse -- Time.Posix, Time.Zone の変換用分岐
    | OnTouchWithTime TouchResponse (Maybe String) Int
-- Msg追加する



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , dbh : Maybe String
    , touchLogs : List TouchData
    , lastTouchLog : Maybe TouchData
    }


type alias TouchData =
    { id : Int
    , idm : String
    , zoneName : String     -- Zone
    , data : Maybe String   -- Icカードの残高
    , createdAt : Int       -- Posix(Int)
    , count : Int           -- 入退室カウント用
    }

defaultTouchLog =
    { id = 0
    , idm = ""
    , zoneName = ""
    , data = Nothing
    , createdAt = 0
    , count = 0
    }

type alias Flags =
    {}



-- FUNCTIONS


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { procModel = Procedure.Program.init
      , config = defaultConfig_pro2
      , dbh = Nothing
      , touchLogs = []
      , lastTouchLog = Nothing
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
    -> Int
    -> String
    -> Cmd Msg
insertTouchCmd idm data zoneName millis dbh =
    let
        sql =
            """
            INSERT INTO TOUCH (
              IDM, DATA, ZONENAME, CREATED
            ) 
            VALUES (?, ?, ?, ?);
            """

        createdAt =
            String.fromInt millis
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
            Json.Decode.succeed TouchData
                |> andMap (Json.Decode.field "ID" Json.Decode.int)
                |> andMap (Json.Decode.field "IDM" Json.Decode.string)
                |> andMap (Json.Decode.field "ZONENAME" Json.Decode.string)
                |> andMap (Json.Decode.field "DATA" (Json.Decode.maybe Json.Decode.string))
                |> andMap (Json.Decode.field "CREATED" Json.Decode.int)
                |> andMap (Json.Decode.field "COUNT" Json.Decode.int)

        decoder =
            Json.Decode.list touchDecoder
    in
    WebSQL.querySQL decoder sql [] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLAllTouch)

-- TouchData DataBase 操作関数

-- IDM, ZONENAME, CREATED, COUNT(IDM), STATUS(IN/OUT) つけてクエリしたやつ。IN/OUT が付くのでこの後自分で計算して出力する必要がない
-- SELECT IDM, ZONENAME, CREATED, CASE WHEN COUNT(IDM) % 2 = 0 THEN 'OUT' ELSE 'IN' END as STATUS,
-- COUNT (IDM) as COUNT FROM TOUCH GROUP BY IDM;

selectLatestWithCount : String -> String -> Cmd Msg
selectLatestWithCount idm dbh =
    let
        andMap =
            Json.Decode.Extra.andMap

        sql =
            """
            SELECT IDM, ZONENAME, CREATED, MAX(CREATED), COUNT(IDM) FROM TOUCH WHERE IDM = ?;
            """

        touchDecoder =
            Json.Decode.succeed TouchData
                |> andMap (Json.Decode.field "ID" Json.Decode.int)
                |> andMap (Json.Decode.field "IDM" Json.Decode.string)
                |> andMap (Json.Decode.field "ZONENAME" Json.Decode.string)
                |> andMap (Json.Decode.field "DATA" (Json.Decode.maybe Json.Decode.string))
                |> andMap (Json.Decode.field "CREATED" Json.Decode.int)
                |> andMap (Json.Decode.field "COUNT" Json.Decode.int)

        decoder =
            Json.Decode.list touchDecoder
    in
    WebSQL.querySQL decoder sql [idm] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (OnError << WebSQLError) SQLAllTouch)

-- SELECT * FROM TOUCH ORDER BY CREATED [DESC] -- 最新のデータ
-- SELECT * FROM TOUCH WHERE CREATED >= 10秒プラスされた時刻


-- UPDATE


filterTouchLogsByIdm : String -> List TouchData -> List TouchData
filterTouchLogsByIdm idm =
    List.filter (\a -> a.idm == idm)


filterTouchLogsByMSec : Int -> List TouchData -> List TouchData
filterTouchLogsByMSec createdAt =
    List.filter (\a -> a.createdAt > createdAt)



-- -> ProcMsg
-- -> OnError
-- -> OnTouch -> OnTouchWithTime　-> SQLDone　-> SQLAllTouch
-- -> GotSetting-> GotDBHandle
-- -> GotDBHandle -> SQLDone　-> SQLAllTouch

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
            ( model, Cmd.none )

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
                |> andMap (Task.map Time.posixToMillis Time.now)
                |> Task.perform identity
            )

        OnTouchWithTime touch zoneName millis ->
            let
                validateTime ms =   -- 10秒はじく from DataBase
                    Maybe.map2 filterTouchLogsByIdm touch.idm (Just model.touchLogs)
                        |> Maybe.map (filterTouchLogsByMSec (ms - 10 * 1000))
                        |> Maybe.map (List.sortBy (.createdAt >> (-) ms))
                        |> Maybe.andThen List.head
                        |> Maybe.Extra.unwrap (Just millis) (always Nothing)
            in
            ( model
            , Cmd.batch
                [ observeTouchCmd model.config
                , Just insertTouchCmd
                    |> Maybe.Extra.andMap touch.idm
                    |> Maybe.Extra.andMap (Maybe.Extra.orElse (Just "") touch.data)
                    |> Maybe.Extra.andMap zoneName
                    |> Maybe.Extra.andMap (validateTime millis)
                    |> Maybe.Extra.andMap model.dbh
                    |> Maybe.withDefault Cmd.none
                , Maybe.map2 selectLatestWithCount touch.idm model.dbh
                    |> Maybe.withDefault Cmd.none
                ]
            )

        GotSetting setting ->
            let
                config =
                    configFromSetting setting
            in
            ( { model | config = config }
            , Cmd.batch
                [ observeTouchCmd model.config
                , createTableCmd
                ]
            )

        GotDBHandle dbh ->
            ( { model | dbh = Just dbh }
            , Task.succeed dbh
                |> Task.perform SQLDone
            )

        SQLAllTouch touchLogs ->
            ( { model
                | touchLogs = touchLogs
                , lastTouchLog = List.head touchLogs
              }
            , Cmd.none
            )

        SQLDone dbh ->
            ( model
            , Maybe.map selectAllTouchCmd model.dbh
                |> Maybe.withDefault Cmd.none
            )



-- VIEW


-- 入室、退室数計算 from DateBase
-- 入室、退室数計算
groupEachIdm data =
    Dict.Extra.groupBy .idm data
        |> Dict.map (\_ v -> List.length v )
{-| 
@docs Dict [(idm, [{userTouchData}])]
@docs Dict [(idm, dataLength)]
-}
     
transformToCounts data =
    Dict.map (\_ v -> (( v + 1 )//2, v//2)) data
{-| 入退室カウント取得
@docs Dict [(idm, (入室回数, 退室回数))]
 -}
totalEntExiCount data =
    Dict.values data
        |> List.unzip
        |> Tuple.mapBoth List.sum List.sum
{-|
@docs [(入室回数, 退室回数)]
@docs ([入室回数], [退室回数])
@docs (入室総数, 退室総数)
 -}



formatTime : Maybe TouchData -> String
formatTime lastTouchLog =
    let
        nameToZone zoneName =
            Dict.get zoneName TimeZone.zones

        touchToZone touch =
            Maybe.map .zoneName lastTouchLog
                |> Maybe.andThen nameToZone
                |> Maybe.map (\zone -> zone ())

        touchToPosix touch =
            Maybe.map .createdAt lastTouchLog
                |> Maybe.map Time.millisToPosix
    in
    Just Time.Format.format
        |> Maybe.Extra.andMap (Just Time.Format.Config.Config_ja_jp.config)
        |> Maybe.Extra.andMap (Just "%Y-%m-%d %H:%M:%S")
        |> Maybe.Extra.andMap (touchToZone lastTouchLog)
        |> Maybe.Extra.andMap (touchToPosix lastTouchLog)
        |> Maybe.withDefault "--/--/-- 00:00:00"


view : Model -> Html Msg
view model =
    let
        idmText =
            model.lastTouchLog
                |> Maybe.map .idm
                |> Maybe.withDefault ""

        deposit =
            model.lastTouchLog
                |> Maybe.andThen .data
                |> Maybe.map (String.slice 20 24)
                |> Maybe.andThen Hex.Convert.toBytes
                |> Maybe.andThen (Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE))
                |> Maybe.withDefault 0
        
        -- 入退室カウント表示用 from DataBase
        entryTimes : List TouchData -> String
        entryTimes data = 
            groupEachIdm data
                |> transformToCounts
                |> Dict.get (Maybe.withDefault defaultTouchLog (model.lastTouchLog)).idm
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
                |> String.fromInt

        exitTimes : List TouchData -> String
        exitTimes data =
            groupEachIdm data
                |> transformToCounts
                |> Dict.get (Maybe.withDefault defaultTouchLog (model.lastTouchLog)).idm
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> String.fromInt

        totalTouchCounts : List TouchData -> String
        totalTouchCounts data =
            groupEachIdm data
                |> Dict.get (Maybe.withDefault defaultTouchLog (model.lastTouchLog)).idm
                |> Maybe.withDefault 0
                |> String.fromInt

        entredNumbers : List TouchData -> String
        entredNumbers logs =
            groupEachIdm logs
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.first
                |> String.fromInt

        exitedNumbers : List TouchData -> String
        exitedNumbers data =
            groupEachIdm data
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.second
                |> String.fromInt

        viewTouchData : Maybe TouchData -> Html msg
        viewTouchData maybeData =
            let
                data = Maybe.withDefault defaultTouchLog maybeData
            in
            li [] [ text <| "IDM : " ++ data.idm ++ " TIME : " ++ (formatTime maybeData) ]

        lastFiveEnteredPeople : List TouchData -> List String
        lastFiveEnteredPeople data =
            List.reverse data
                |> groupEachIdm
                |> Dict.toList
                |> List.take 4
                |> List.unzip
                |> Tuple.first

        viewLastFiveEnteredPeople : String -> Html msg
        viewLastFiveEnteredPeople data =
            li [] [ text <| "IDM : " ++ data ]
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ text idmText ] ]
        , div [] [ p [] [ text <| formatTime model.lastTouchLog ] ]
        , div [] [ p [] [ text <| String.fromInt deposit ] ]
        , div [] [ p [] [ text <| "Enter : " ++ entryTimes model.touchLogs ++ " times "
                        , text <| "Exit : " ++ exitTimes model.touchLogs ++ " times "
                        , text <| "Total Counts : " ++ totalTouchCounts model.touchLogs ++ " times "
                        ]]
        , div [] [ p [] [ text <| "Entred People : " ++ entredNumbers model.touchLogs ++ " times "
                        , text <| "Exited People : " ++ exitedNumbers model.touchLogs ++ " times "]]
        , div [] [ ul [] ( List.map viewLastFiveEnteredPeople (lastFiveEnteredPeople model.touchLogs)) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Procedure.Program.subscriptions model.procModel
        ]
