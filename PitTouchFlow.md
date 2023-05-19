::: mermaid
sequenceDiagram
    autonumber
    actor ユーザー
    participant pittouch as PitTouch
    participant db as DB
    pittouch->>pittouch: 初期設定情報取得
    pittouch->>db: テーブル作成
    pittouch->>pittouch: タッチを一度だけ観測するためのポーリング開始
    ユーザー->>pittouch: タッチ
    Note left of pittouch: タッチデータ取得(TouchResponse)
    pittouch->>pittouch: Zone, Posix 計算
    pittouch->>pittouch: TouchResponseと、計算した Zone, Posix を次のMsgに渡す
    pittouch->>db: IDMとPosixでDB問い合わせ
    alt 10秒以内なし
        db->>pittouch: DBから取得できたレコードなし(0レコード)
        pittouch->>db: DBにインサート
        Note left of db: IDM, DATA, ZONE, POSIXを保存
    else 10秒以内あり
        db->>pittouch: DBから取得できたレコードあり(1レコード)
        Note left of db: ID, IDM, DATA, ZONE, POSIXの1レコードが取得できる
        pittouch->>pittouch: 何もしない
    end
    pittouch->>db: IDMでDB問い合わせ(カウント取得)
    db->>pittouch: 検索結果を返す
    Note left of db: INCOUNT, OUTCOUNT, TOTALCOUNT をBDで計算
    pittouch->>ユーザー: IDM, 時間, 残金, 入室回数, 退室回数, 合計入退室回数 をユーザーへ表示
    pittouch->>pittouch: タッチを一度だけ観測するためのポーリング開始
    

%% ```mermaid
%%  この中にMermaid記法
%% ```
:::
DBへ入れるもの：IDM, DATA, ZONE, POSIX, DBH

DBから取得できるもの：ID, IDM, DATA, ZONE, POSIX
