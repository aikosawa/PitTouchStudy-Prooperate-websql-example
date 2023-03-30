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
    | GotCurrentInOut TouchResponse (Maybe String) Int (List TouchData)



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , dbh : Maybe String
    , touchLogs : List TouchData
    , lastTouchLog : Maybe TouchData
    , touchLogWithCount : List TouchData
    }


type alias TouchData =
    { id : Int
    , idm : String
    , zoneName : String     -- Zone
    , data : Maybe String   -- Icカードの残高
    , createdAt : Int       -- Posix(Int)
    , count : Int           -- 入退室カウント用
    , incount : Int
    , outcount : Int
    }

defaultTouchLog =
    { id = 0
    , idm = ""
    , zoneName = ""
    , data = Nothing
    , createdAt = 0
    , count = 0
    , incount = 0
    , outcount = 0
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
      , touchLogWithCount = []
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
            SELECT ID, IDM, ZONENAME, DATA, CREATED,
            0 AS COUNT, 0 AS INCOUNT, 0 AS OUTCOUNT
            FROM TOUCH ORDER BY CREATED DESC;
            """

        touchDecoder =
            Json.Decode.succeed TouchData
                |> andMap (Json.Decode.field "ID" Json.Decode.int)
                |> andMap (Json.Decode.field "IDM" Json.Decode.string)
                |> andMap (Json.Decode.field "ZONENAME" Json.Decode.string)
                |> andMap (Json.Decode.field "DATA" (Json.Decode.maybe Json.Decode.string))
                |> andMap (Json.Decode.field "CREATED" Json.Decode.int)
                |> andMap (Json.Decode.field "COUNT" Json.Decode.int)
                |> andMap (Json.Decode.field "INCOUNT" Json.Decode.int)
                |> andMap (Json.Decode.field "OUTCOUNT" Json.Decode.int)

        decoder =
            Json.Decode.list touchDecoder

    in
    WebSQL.querySQL decoder sql [] dbh
        |> Procedure.try ProcMsg
            (Result.Extra.unpack (WebSQLError >> OnError) SQLAllTouch)

-- TouchData DB操作関数
selectLatestRecordWithin10Secs : String -> Int -> Int -> Task Http.Error (List TouchData)
-- selectLatestRecordWithin10Secs : String -> TouchResponse -> Maybe String -> Int -> Cmd Msg
selectLatestRecordWithin10Secs dbh idm millis =
    let
        andMap =
            Json.Decode.Extra.andMap

        sql =
            -- """
            -- SELECT ID, IDM, ZONENAME, DATA, CREATED, 
            -- COUNT(IDM) / 2 AS INCOUNT,
            -- (COUNT(IDM) + 1) / 2 AS OUTCOUNT, 
            -- CASE WHEN COUNT(IDM) % 2 = 0 THEN 'OUT' ELSE 'IN' END as INOUT,
            -- CASE WHEN ? > (CREATED + (10 * 1000))  THEN 'TRUE' ElSE 'FALSE' END as STATUS,
            -- COUNT(IDM) as COUNT
            -- FROM TOUCH WHERE IDM = ?
            -- ORDER BY CREATED DESC
            -- LIMIT 1;
            -- """

            """
            SELECT ID, IDM, ZONENAME, DATA, CREATED, 
            COUNT(IDM) as COUNT,
            COUNT(IDM) / 2 AS INCOUNT,
            (COUNT(IDM) + 1) / 2 AS OUTCOUNT            
            FROM TOUCH WHERE IDM = '?' AND ? < (CREATED + (10 * 1000))
            ORDER BY CREATED DESC
            LIMIT 1;
            """

        touchDecoder =
            Json.Decode.succeed TouchData
                |> andMap (Json.Decode.field "ID" Json.Decode.int)
                |> andMap (Json.Decode.field "IDM" Json.Decode.string)
                |> andMap (Json.Decode.field "ZONENAME" Json.Decode.string)
                |> andMap (Json.Decode.field "DATA" (Json.Decode.maybe Json.Decode.string))
                |> andMap (Json.Decode.field "CREATED" Json.Decode.int)
                |> andMap (Json.Decode.field "COUNT" Json.Decode.int)
                |> andMap (Json.Decode.field "INCOUNT" Json.Decode.int)
                |> andMap (Json.Decode.field "OUTCOUNT" Json.Decode.int)
                
        decoder =
            Json.Decode.list touchDecoder

        createdAt =
            String.fromInt millis
    in
    -- WebSQL.querySQL decoder sql [ createdAt, (Maybe.withDefault "" touch.idm )] dbh
    --     |> Procedure.try ProcMsg (Result.Extra.unpack (WebSQLError >> OnError) (GotCurrentInOut touch zoneName millis))
    WebSQL.querySQL decoder sql [ idm, createdAt ] dbh
        |> Procedure.try ProcMsg (Result.Extra.unpack (WebSQLError >> OnError) Identity)
    



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

                millis = 
                    Task.map Time.posixToMillis Time.now
            in
            ( model
            , Task.map2 Tuple.pair millis (selectLatestRecordWithin10Secs model.dbh (Maybe.withDefault "" touch.idm ) millis)
                |> Task.map (\(millis, touchLog) -> OnTouchWithTime (touch, (getZoneName touch), millis, touchLog))
                |> Task.perform OnError

            , Task.map Time.posixToMillis Time.now
                |> Task.andThen
                    (\millis ->
                        Task.map2 Tuple.pair (Task.succeed millis)(selectLatestRecordWithin10Secs model.dbh (Maybe.withDefault "" touch.idm ) millis)
                            |> Task.map (\(millis, touchLog) -> OnTouchWithTime (touch, getZoneName, millis, touchLog))
                            |> Task.perform OnError
                    )
            )

        OnTouchWithTime touch zoneName millis ->
            ( model
            , Cmd.batch
                [ observeTouchCmd model.config
                , Just selectLatestRecordWithin10Secs 
                    |> Maybe.Extra.andMap model.dbh
                    |> Maybe.Extra.andMap (Just touch)
                    |> Maybe.Extra.andMap (Just zoneName)
                    |> Maybe.Extra.andMap (Just millis)
                    |> Maybe.withDefault Cmd.none
                ]
            )

        GotCurrentInOut touch zoneName millis touchLog ->
            ( { model | touchLogWithCount = touchLog }
            , Just insertTouchCmd
                |> Maybe.Extra.andMap touch.idm
                |> Maybe.Extra.andMap (Maybe.Extra.orElse (Just "") touch.data)
                |> Maybe.Extra.andMap zoneName
                |> Maybe.Extra.andMap (Just millis)
                |> Maybe.Extra.andMap model.dbh
                |> Maybe.withDefault Cmd.none
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
        
        -- 入退室カウント表示用 from DB
        -- entredNumbers : List TouchData -> String
        -- entredNumbers logs =
        --     groupEachIdm logs
        --         |> transformToCounts
        --         |> totalEntExiCount
        --         |> Tuple.first
        --         |> String.fromInt

        -- exitedNumbers : List TouchData -> String
        -- exitedNumbers data =
        --     groupEachIdm data
        --         |> transformToCounts
        --         |> totalEntExiCount
        --         |> Tuple.second
        --         |> String.fromInt

        viewTouchData : Maybe TouchData -> Html msg
        viewTouchData maybeData =
            let
                data = Maybe.withDefault defaultTouchLog maybeData
            in
            li [] [ text <| "IDM : " ++ data.idm ++ " TIME : " ++ (formatTime maybeData) ]

        -- lastFiveEnteredPeople : List TouchData -> List String
        -- lastFiveEnteredPeople data =
        --     List.reverse data
        --         |> groupEachIdm
        --         |> Dict.toList
        --         |> List.take 4
        --         |> List.unzip
        --         |> Tuple.first

        viewLastFiveEnteredPeople : String -> Html msg
        viewLastFiveEnteredPeople data =
            li [] [ text <| "IDM : " ++ data ]
    
        maybeDefault : (TouchData -> Int) -> String
        maybeDefault cons = 
            List.head model.touchLogWithCount
            |> Maybe.withDefault defaultTouchLog
            |> cons
            |> String.fromInt
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ text idmText ] ]
        , div [] [ p [] [ text <| formatTime model.lastTouchLog ] ]
        , div [] [ p [] [ text <| String.fromInt deposit ] ]
        , div [] [ p [] [ text <| "Enter : " ++  maybeDefault .incount  ++ " times "
                        , text <| "Exit : "  ++  maybeDefault .outcount  ++ " times "
                        , text <| "Total Counts : " ++  maybeDefault .count  ++ " times "
                        ]]
        -- , div [] [ p [] [ text <| "Entred People : " ++ entredNumbers model.touchLogs ++ " times "
        --                 , text <| "Exited People : " ++ exitedNumbers model.touchLogs ++ " times "]]
        -- , div [] [ ul [] ( List.map viewLastFiveEnteredPeople (lastFiveEnteredPeople model.touchLogs)) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Procedure.Program.subscriptions model.procModel
        ]
