::: mermaid
sequenceDiagram
    autonumber
    actor ユーザー
    participant pittouch as PitTouch
    participant prsys as 処理系
    pittouch->>+prsys: Cmd 初期設定情報
    prsys -->>-pittouch: 
    Note left of prsys: setting.json
    pittouch->>+prsys: Cmd テーブル作成
    prsys -->>-pittouch: 
    Note left of prsys: テーブル作成成功
    pittouch->>+prsys: Cmd ポーリング開始
    prsys -->>-pittouch: 
    Note left of prsys: ポーリング開始成功
    ユーザー->>pittouch: タッチ
    pittouch->>+prsys: Cmd タッチデータ取得(TouchResponse)
    prsys -->>-pittouch: 
    Note left of prsys: タッチデータ
    pittouch->>+prsys: Cmd Zone, Posix取得
    prsys -->>-pittouch: 
    Note left of prsys: Zone, Posix
    pittouch->>pittouch: TouchResponse, Zone, Posix をModelに保存
    pittouch->>+prsys: Cmd 最新のレコードDB問い合わせ
    prsys -->>-pittouch: 
    Note left of prsys: 問い合わせ結果
    pittouch->>pittouch: 10秒比較
    alt 10秒以内なし
        pittouch->>+prsys: Cmd DBにタッチデータインサート
        prsys -->>-pittouch: 
        Note left of prsys: インサート成功
    else 10秒以内あり
        pittouch->>pittouch: 何もしない
    end
    pittouch->>+prsys: Cmd DBに全ての入退室カウント計算、取得 問い合わせ
    prsys -->>-pittouch: 
    Note left of prsys: 問い合わせ結果
    pittouch->>+prsys: Cmd ポーリング開始
    prsys -->>-pittouch: 
    Note left of prsys: ポーリング開始成功
    pittouch ->> ユーザー: カウント表示 
    

%% ```mermaid
%%  この中にMermaid記法
%% ```
:::
DBへ入れるもの：IDM, DATA, ZONE, POSIX, DBH

DBから取得できるもの：ID, IDM, DATA, ZONE, POSIX
