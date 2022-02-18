port module QVGame1 exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events.Extra.Mouse as Mouse
import Svg
import Svg.Attributes exposing (d, stroke, fill, strokeWidth, width, height, viewBox, x, y)
import Dict
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)

import Html.Events exposing (onClick, stopPropagationOn, targetValue)
import Json.Decode as Decode
import Json.Encode as Encode

import File
import File.Download as Download
import File.Select as Select
import Task


--送信用ポート
port sendMes : Encode.Value -> Cmd msg
--受信用ポート
port receiveMes : (Decode.Value -> msg) -> Sub msg



-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
            


-- MODEL 

type alias Magnitude = Float

type alias Position = (Magnitude, Magnitude)

type BrickType 
    = BasicBrick
    | EntryBrick
    | TailBrick
    | CaseBrick    

type BrickCommand 
    = CommandNOP -- "動作開始"
    | CommandMove -- "n歩前進する"
    | CommandTurnRight -- "n度右回りする"
    | CommandTurnLeft -- "n度左回りする"
    | CommandTurnBack -- "左右反転する"
    | CommandFuncStart -- "関数nを開始"
    | CommandFuncStop -- "関数nへ移動"
    | CommandIfS
    | CommandIfR
    | CommandIfL
    | CommandIfB
    | CommandNone
    | CommandReboot

-- 引数情報
type alias Argument
    = { disp : String --引数の有無 及び 引数の横に表示する文字列
      , value : String --引数
      }

type alias Node
    = { getBrickType : BrickType --ブロックの種類(色と形状)
      , getBrickCommand : BrickCommand --ブロックの内容
      , getBrickArgument : Argument --引数情報
      , getBrickTab : Int --表示エリアのタブ番号
      }

-- 抽象構文木（AST）
type AST a
    = Nil
    | AST a
          (AST a) -- bottom (left)
          (AST a) -- right

-- 非空（non-empty）のAST
type ASTne a
    = ASTne a (AST a) (AST a)

-- 根の位置情報付きのAST
type ASTxy a
    = ASTxy 
        Position -- 根の座標
        (ASTne a)              -- 非空に限定 

--DnD時の情報
type alias DnDInfo 
    = { getOnDnD : Bool -- DnDの最中は真 
      , getBrickTab : Int  --移動中ブロックのタブ番号
      , getXY0 : Position  -- DnD開始時点のマウスの座標（event.offsetPos）
      , getBrickXY : Position -- 移動中ブロックの移動前の座標
      , getMoveXY : Position -- ドラッグ中のブロックの座標
      }
      
--Model型
type alias Model 
    = { getBrickSize : Magnitude --ブロックのサイズ
      , getASTRoots : List (ASTxy Node) --作成プログラム 
      , getDnDInfo : DnDInfo --DnD情報
      , nowTabE : Int --エディタのタブ番号
      , nowTabP : Int --パレットのタブ番号
      }

-- ブロック間の間隔
-- 凹凸の分を除くのでブロックサイズの90%
interval : Model -> Magnitude
interval model = model.getBrickSize * 0.9

-- 位置比較時のマージン
mergin : Magnitude
mergin = 20

-- ブロックの初期配置
setBrick : List (ASTxy Node)
setBrick =
  let
    pos = (450, 100)
  in
    [ ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandNOP
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandFuncStart
            , getBrickArgument = {disp = "n = ", value = "1"}
            , getBrickTab = 1
            } Nil Nil
        )
    , ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandFuncStart
            , getBrickArgument = {disp = "n = ", value = "2"}
            , getBrickTab = 2
            } Nil Nil
        )
    , ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandFuncStart
            , getBrickArgument = {disp = "n = ", value = "3"}
            , getBrickTab = 3
            } Nil Nil
        )
    , ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandFuncStart
            , getBrickArgument = {disp = "n = ", value = "4"}
            , getBrickTab = 4
            } Nil Nil
        )
    , ASTxy pos
        ( ASTne 
            { getBrickType = EntryBrick
            , getBrickCommand = CommandFuncStart
            , getBrickArgument = {disp = "n = ", value = "5"}
            , getBrickTab = 5
            } Nil Nil
        )
    ]

--パレット上のブロック情報
setPallet : List (ASTxy Node)
setPallet =
  let
    x = 80
    y = 100
  in
    [ ASTxy
        (x, y)
        ( ASTne 
            { getBrickType = BasicBrick
            , getBrickCommand = CommandMove
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy
        (x+150, y)
        ( ASTne 
            { getBrickType = BasicBrick
            , getBrickCommand = CommandTurnBack
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy
        (x, y+150)
        ( ASTne 
            { getBrickType = BasicBrick
            , getBrickCommand = CommandTurnLeft
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy
        (x+150, y+150)
        ( ASTne 
            { getBrickType = BasicBrick
            , getBrickCommand = CommandTurnRight
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy
        (x, y+300)
        ( ASTne 
            { getBrickType = BasicBrick
            , getBrickCommand = CommandNone
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 0
            } Nil Nil
        )
    , ASTxy
        (x, y)
        ( ASTne 
            { getBrickType = CaseBrick
            , getBrickCommand = CommandIfS
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 1
            } Nil Nil
        )
    , ASTxy
        (x+150, y)
        ( ASTne 
            { getBrickType = CaseBrick
            , getBrickCommand = CommandIfB
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 1
            } Nil Nil
        )
    , ASTxy
        (x, y+150)
        ( ASTne 
            { getBrickType = CaseBrick
            , getBrickCommand = CommandIfL
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 1
            } Nil Nil
        )
    , ASTxy
        (x+150, y+150)
        ( ASTne 
            { getBrickType = CaseBrick
            , getBrickCommand = CommandIfR
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 1
            } Nil Nil
        )
    , ASTxy
        (x, y)
        ( ASTne 
            { getBrickType = TailBrick
            , getBrickCommand = CommandFuncStop
            , getBrickArgument = {disp = "n = ", value = ""}
            , getBrickTab = 2
            } Nil Nil
        )
    , ASTxy
        (x, y+150)
        ( ASTne 
            { getBrickType = TailBrick
            , getBrickCommand = CommandReboot
            , getBrickArgument = {disp = "", value = ""}
            , getBrickTab = 2
            } Nil Nil
        )
    ]

--init関数
init : () -> (Model, Cmd Msg)
init flags = (
    { getBrickSize = 128
    , getASTRoots = setBrick --初期状態を格納
    , getDnDInfo = 
        { getOnDnD = False -- DnDの最中かどうか
        , getBrickTab = -1
        , getXY0 = (0, 0) -- DnD中のoffsetPos
        , getBrickXY = (0, 0) -- DnD対象ブロックの移動前の座標
        , getMoveXY = (0, 0) -- DnD中のマウスの座標(clientPos)
        }
    , nowTabE = 0
    , nowTabP = 0
    }   
    , Cmd.none )



-- エンコード用 BrikcTypeを文字列に変換
encodeBrickType : BrickType -> Encode.Value
encodeBrickType bt =
  Encode.string (
    case bt of
      EntryBrick -> "EntryBrick"
      BasicBrick -> "BasicBrick"
      TailBrick -> "TailBrick"
      CaseBrick -> "CaseBrick"
  )
   
-- エンコード用 BrickCommandを文字列に変換
encodeBrickCommand : BrickCommand -> Encode.Value
encodeBrickCommand bc =
  Encode.string (
    case bc of
      CommandNOP -> "CommandNOP"
      CommandMove -> "CommandMove"
      CommandTurnRight -> "CommandTurnRight"
      CommandTurnLeft -> "CommandTurnLeft"
      CommandTurnBack -> "CommandTurnBack"
      CommandFuncStart -> "CommandFuncStart"
      CommandFuncStop -> "CommandFuncStop"
      CommandIfS -> "CommandIfS"
      CommandIfR -> "CommandIfR"
      CommandIfL -> "CommandIfL"
      CommandIfB -> "CommandIfB"
      CommandNone -> "CommandNone"
      CommandReboot -> "CommandReboot"
  )

-- ブロック単位のエンコード
-- CaseBrickの場合、Argument.dispも追加して前置記法の形にする
encodeAST : AST Node -> Encode.Value
encodeAST ast =
  case ast of
    Nil ->
      Encode.string "Nil"
    AST node bottom right ->
      Encode.object
        [ ( "node", Encode.object
            [ ( "getBrickType", encodeBrickType node.getBrickType)
            , ( "getBrickCommand", encodeBrickCommand node.getBrickCommand)
            , ( "getBrickArgument", Encode.object
                [ ("disp", Encode.string node.getBrickArgument.disp)
                , ("value", Encode.string node.getBrickArgument.value)
                ])
            , ( "getBrickTab", Encode.int node.getBrickTab)
            ])
        , ("bottom", encodeAST bottom)
        , ("right", encodeAST right)
        ]

-- 構文木単位のエンコード
-- 葉は全てencodeASTでエンコードする
encodeASTxy : ASTxy Node -> Encode.Value
encodeASTxy (ASTxy (posx, posy) (ASTne node astb astr)) =
  Encode.object
    [ ("position", Encode.object [("x", Encode.float posx), ("y", Encode.float posy)] )
    , ("ASTne", encodeAST (AST node astb astr) )
    ]

-- 全体のエンコード
-- 構文木をエンコードしたものをリスト化し、そのリストをエンコードする
encodeASTRoots : Model -> Encode.Value
encodeASTRoots model =
    List.map encodeASTxy model.getASTRoots
    |> Encode.list (\a -> a) 



-- UPDATE

type Msg = MsgCloneUs (ASTne Node) Mouse.Event
         | MsgLetMeRoot (ASTne Node) Position Mouse.Event
         | MsgMoveUs Mouse.Event
         | MsgAttachMe (ASTxy Node) Position Mouse.Event
         | MsgStartDnD (ASTxy Node) Mouse.Event         
         | MsgInputChanged Position String String -- 引数情報の更新
         | Dummy Mouse.Event -- 何もしない
         | MsgEncoding -- プログラムをJSONにエンコードする
         | MsgGetMessage Decode.Value -- Htmlから情報を受け取る
         | MsgCheckReset -- プログラム削除の確認
         | MsgChangeTab Int Bool -- タブ切り替え
         | MsgLoadFile -- jsonファイルを開く
         | MsgGotFile File.File -- ファイルの内容を獲得する
         | MsgGotString String -- ファイルから獲得した情報を変換する
         | MsgSaveFile -- プログラム保存の確認

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        MsgCloneUs ast event -> 
            (cloneUs ast event model, Cmd.none)
        -- 根ではMsgLetMeRootは生じないので単独のMsgStartDnDは必要
        MsgStartDnD (ASTxy xy (ASTne n b r)) event ->
            if n.getBrickType == EntryBrick || model.getDnDInfo.getOnDnD then (model, Cmd.none)
            else (startDnDxy (ASTxy xy (ASTne n b r)) event model, Cmd.none)
        MsgLetMeRoot ast xy event ->
            (letMeRoot ast event model |> startDnD event, Cmd.none)
        MsgMoveUs event ->
            (model |> moveUs event, Cmd.none)
        MsgAttachMe astxy xy1 event ->
            (attachMe astxy xy1 event model |> stopDnD event, Cmd.none)
        -- 引数情報の更新
        MsgInputChanged xy argCode value ->
            (inputValuexy xy argCode value model, Cmd.none)
        -- 実質なにもしない stopDnDはバグ回避用
        Dummy event ->
            (stopDnD event model, Cmd.none)
        -- エンコードした情報をhtmlに送る
        MsgEncoding ->
            (model, sendMes (encodeASTRoots model))
        -- htmlから情報を受け取る
        MsgGetMessage command ->
            case Decode.decodeValue Decode.string command of
                --受信した情報が文字列(保存するファイルの名前)
                Ok fileName ->
                    ( model
                    , Encode.encode 0 (encodeASTRoots model)
                      |> Download.string fileName "text/json"
                    )
                Err _ ->
                    case Decode.decodeValue Decode.bool command of
                        --受信した情報がTrue(プログラム削除の決定)
                        Ok True ->
                            ({ model | getASTRoots = setBrick }, Cmd.none)
                        _ ->
                            (model, Cmd.none)
        --プログラム削除の確認メッセージを送信
        MsgCheckReset ->
            (model, sendMes (Encode.string "Reset"))
        --タブ番号を更新
        MsgChangeTab num te ->
            if te then ({model | nowTabP = num}, Cmd.none) --パレット
            else ({model | nowTabE = num}, Cmd.none) --エディタ
        -- 1.jsonファイルを開く
        MsgLoadFile ->
            (model, Select.file ["text/json"] MsgGotFile)
        -- 2.ファイルの内容を獲得する
        MsgGotFile file ->
            (model, Task.perform MsgGotString (File.toString file))
        -- 3.ファイルから獲得した情報を変換する
        MsgGotString str ->
            case Decode.decodeString (Decode.list Decode.value) str of
              Ok jsonData ->
                ( { model | getASTRoots = List.map decodeASTxy jsonData }
                , Cmd.none)
              Err _ ->
                (model, Cmd.none)
        --プログラム保存の確認メッセージの送信
        MsgSaveFile ->
            (model, sendMes (Encode.string "Save"))

-- ASTへの処理で更新があったかどうかの判定で使用
type Change = Changed | Unchanged

cor : Change -> Change -> Change
cor a b = 
    case (a, b) of
        (Unchanged, Unchanged) -> 
            Unchanged
        (_ ,_ ) ->
            Changed   

-- Writerアプリカティブ
-- elmには型クラスがないのでChangeモノイドに特化
type alias W a = (a, Change)

-- 使用例: 
-- absW x = if x > 0 then (x, Unchanged) else (-x, Changed)
-- unit (+) |> andMap (absW 1) |> andMap (absW -2)  ==> (3, Changed)
-- unit (+) |> andMap (absW 1) |> andMap (absW 2)   ==> (3, Unchanged)
andMap : W a -> W (a -> b) -> W b
andMap (a, c) (f, d) = (f a, cor c d)

unit : a -> W a
unit a = (a, Unchanged)

-- リストの各要素に関数fを作用させ（List.mapと同じ），その過程で
-- 変更が生じたかどうかを合わせて返す
-- 使用例：
-- absW x = if x > 0 then (x, Unchanged) else (-x, Changed)
-- listMapW absW [1, -2, 3, -4, 5]  ==>  ([1, 2, 3, 4, 5], Changed)
-- listMapW absW [1, 2, 3, 4, 5]    ==>  ([1, 2, 3, 4, 5], Unchanged)
listMapW : (a -> W a) -> List a -> W (List a) 
listMapW f = List.foldr (\a mbs -> unit (::) |> andMap (f a) |> andMap mbs)
                        (unit [])           

-- ペア（2組）の各要素毎に2項演算を作用させる
-- 使用例： pairMap2 (+) (1, 10) (2, 20)  ==>  (3, 30)
pairMap2 : (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
pairMap2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)
    
-- 抽象構文木(AST)への再帰的処理のひな形
-- 以下の処理を葉から根にかけてボトムアップに適用する
-- - 葉には初期値を与える関数uを適用
-- - 葉以外のノードには以下の4引数の関数fを適用
-- f(d,n,b,r)の各引数の意味：
-- d: 各ブロック間の間隔
-- n: 葉以外の各ノード
-- b: サブAST bに対する処理済みの結果
-- r: サブAST rに対する処理済みの結果
-- 使用例： 
-- recurAST 0 (\_ a b c -> a + b + c) (\_ -> 0) (0,0) (AST 1 (AST 2 Nil Nil) (AST 3 Nil Nil)) 
-- ==> 6 (= 1 + 2 + 3)
recurAST : Magnitude -> 
           (Position -> a -> acc -> acc -> acc) -> 
           (Position -> acc) ->
           Position -> AST a -> acc
recurAST d f u (x, y) ast = 
    case ast of
        Nil -> 
            u (x, y)
        AST node bottom right ->     
            f (x, y)
              node
              (recurAST d f u (x, y + d) bottom)
              (recurAST d f u (x + d, y) right)
                    
-- モデルに（根の位置情報付きの）ASTを追加
addASTxy : ASTxy Node -> Model -> Model
addASTxy astxy model = 
        { model | getASTRoots = model.getASTRoots ++ [astxy] }

-- モデルから（根の位置情報付きの）ASTを削除
-- elmは参照透明なので等価性の判定は参照同値ではなく構造同値であることに注意
-- 実際は根の位置情報の食い違いで非等価がすぐに分かるのでAST全体を調べることはまれ
removeAST : ASTxy Node -> Model -> Model
removeAST astxy model =
    { model | getASTRoots = 
                model.getASTRoots |> List.filter (\a -> a /= astxy) 
    }    

-- サブASTの複製を追加
-- 純粋関数型言語であるElmではあらゆるデータは不変（persistent）なので
-- 実際にはASTの複製をつくる必要がないことに注意
-- 単にクリックしたマウスの座標情報を付加した上で元のサブASTを共有するだけ
-- でよいので非常に効率的で軽い処理 
cloneUs : ASTne Node -> Mouse.Event -> Model -> Model
cloneUs (ASTne n b r) event model =
    if event.button == Mouse.MainButton || model.getDnDInfo.getOnDnD || (n.getBrickType == EntryBrick) then
        model
    else
        --addASTxy (ASTxy (pairMap2 (+) 
        --                  (pairMap2 (-) event.pagePos event.offsetPos) --凹凸を含む左上の角の座標
        --                  (30, 30)) -- 元のブロックと完全に重ならないように少しずらす
        --                (ASTne n b r))
        --         model
        rePos (ASTxy (pairMap2 (+) 
                          (pairMap2 (-) event.pagePos event.offsetPos) --凹凸を含む左上の角の座標
                          (30, 30)) -- 元のブロックと完全に重ならないように少しずらす
                        (ASTne n b r))
              True model

-- ASTの移動
-- 単に根の位置情報を変更するだけでよいので非常に軽い処理
moveUs : Mouse.Event -> Model -> Model
moveUs event model = 
    if not model.getDnDInfo.getOnDnD then 
        -- MsgAttachMeの直後にMsgMoveUsが生じてブリックが不作為にリープするバグへの対処
        -- おそらくメッセージがキューされていて，タイミングが悪いとDnD終了後であっても
        -- MsgAttachMeが生じるものと思われる．
        model
    else    
        { model | getDnDInfo =
            { getOnDnD = model.getDnDInfo.getOnDnD
            , getBrickTab = model.getDnDInfo.getBrickTab
            , getXY0 = model.getDnDInfo.getXY0
            , getBrickXY = model.getDnDInfo.getBrickXY
            , getMoveXY = pairMap2 (-) event.clientPos model.getDnDInfo.getXY0
            }
        }

insideBrick : Position -> Position -> Bool
insideBrick (x0, y0) (x, y) = 
    y0 - mergin <= y && y <= y0 + mergin &&
    x0 - mergin <= x && x <= x0 + mergin 

-- ドラッグアンドドロップ（DnD）の開始をモデルに記録
startDnD : Mouse.Event -> (Model, Position, Int) -> Model
startDnD event (model, xy, t) = 
    if event.button /= Mouse.MainButton  then
        model
    else     
        { model | getDnDInfo = { getOnDnD = True
                               , getBrickTab = t
                               , getXY0 = pairMap2 (-) event.clientPos xy --event.offsetPos
                               , getBrickXY = xy
                               , getMoveXY = xy --pairMap2 (-) event.clientPos  event.offsetPos
                               } }

-- 根専用のDnD開始処理
--再表示をすることによってブロック群を最前面に表示させる
startDnDxy : ASTxy Node -> Mouse.Event -> Model -> Model
startDnDxy (ASTxy xy (ASTne node bottom right)) event model = 
    if event.button /= Mouse.MainButton  then
        model
    else
        { model | getDnDInfo = { getOnDnD = True
                               , getBrickTab = node.getBrickTab
                               , getXY0 = pairMap2 (-) event.clientPos xy --event.offsetPos
                               , getBrickXY = xy
                               , getMoveXY = xy --pairMap2 (-) event.clientPos  event.offsetPos
                               } }
        |> removeAST (ASTxy xy (ASTne node bottom right))
        |> addASTxy (ASTxy xy (ASTne { node | getBrickTab = model.nowTabE } bottom right))   

-- ドラッグアンドドロップ（DnD）の終了をモデルに記録
stopDnD : Mouse.Event -> Model -> Model
stopDnD event model =
    if event.button /= Mouse.MainButton  then
        model
    else   
        -- getXY0 = (0, 0)は不要だが，デバッガで見やすいように0をセット
        { model | getDnDInfo = { getOnDnD = False, getBrickTab = -1, getXY0 = (0, 0), getBrickXY = (0, 0), getMoveXY = (0, 0) } }

-- マウスでクリックした位置のブロックを根とするASTをモデルに追加する
-- 元のASTがすべて作り直される重い処理
-- 順序木への挿入処理のように本当は根に至る最小限の経路のみ作り直したいが，
-- クリック位置情報だけからはどちらの部分木の配下が更新されるのか判定できない
-- のでやむを得ない
letMeRoot : ASTne Node -> Mouse.Event -> Model -> (Model, Position, Int)
letMeRoot (ASTne node bottom right) event model = 
    if event.button /= Mouse.MainButton || model.getDnDInfo.getOnDnD 
    then
        (model, pairMap2 (-) event.pagePos event.offsetPos, node.getBrickTab)
    else     
        let
            -- 新規のルートポジション
            xy =  pairMap2 (-) event.pagePos event.offsetPos

            -- 切り離すブロックの番号
            t = node.getBrickTab
                
            f xy0 n b r =
                if insideBrick xy0 xy && n.getBrickTab == t then 
                    Nil -- 部分ASTをNilで置換
                else
                    AST n b r -- すべてのノードを作り直し
        in
            ({ model | getASTRoots = 
                model.getASTRoots 
                |> List.map (\(ASTxy (x, y) (ASTne n b r)) ->
                        let d = interval model 
                        in ASTxy (x, y)
                            (ASTne n
                                (recurAST d f (\_ -> Nil) (x, y + d) b)
                                (recurAST d f (\_ -> Nil) (x + d, y) r)
                            )
                    )
            }                                        
            |> addASTxy (ASTxy xy (ASTne node bottom right)) 
            , xy
            , node.getBrickTab)

--追加、recurASTを変更。rescurASTはletMeRootにも使われているためattachMe専用のrescurASTを作成
recurAttachAST : Magnitude -> 
           (Position -> Node -> W (AST Node) -> W (AST Node) -> W (AST Node)) -> 
           (Position -> W (AST Node)) ->
           Position -> AST Node -> W (AST Node)
recurAttachAST d f u (x, y) ast = 
    case ast of
        Nil -> 
            u (x, y)
        AST node bottom right ->
            if node.getBrickType /= TailBrick then --変更、探索途中にTailBrickがあるかどうかを判定
            f (x, y)
              node
              (recurAttachAST d f u (x, y + d) bottom)
              (recurAttachAST d f u (x + d, y) right)
              else  unit (AST node bottom right)--変更、探索途中にTailがあった場合何も変更を加えない処理を行う
              
-- ASTを別のASTの葉に接木する
-- かならずしも接木が行われるとは限らない（近接ブロックがない位置でマウスをリリースするかもしれない）
-- よって接木が成功したことを確認した上ではじめてAST astをASTのリストから削除すべきことに注意
-- letMeRootと同様，すべてのASTを作り直すことになる重い処理（接木したい位置情報からだけでは
-- どちらの部分木配下が対象になるのか判定しようがないので致し方ない）
attachMe : ASTxy Node -> Position -> Mouse.Event -> Model -> Model
attachMe (ASTxy xy (ASTne node bottom right)) xy1 event model = 
  if event.button /= Mouse.MainButton || not model.getDnDInfo.getOnDnD then model
  else
    let 
        model1 = removeAST (ASTxy xy1 (ASTne node bottom right)) model
        -- 接続できたときのみastをルートリストから削除するために
        -- Writerアプリカティブで変更の有無を判定
        u xy0 = 
          if insideBrick xy0 xy then 
                (AST node bottom right, Changed)
          else
                unit Nil

        f _ n b r =
            unit (AST n) |> andMap b |> andMap r

        t = node.getBrickTab
    in    
        if node.getBrickType == EntryBrick then
            rePos (ASTxy xy (ASTne node bottom right)) False model1
        else
            let 
                (newRoots, isChanged) = 
                 listMapW  
                     (
                         (\(ASTxy (x, y) (ASTne n b r)) ->
                         if n.getBrickType /= TailBrick && n.getBrickTab == t then --追加、根のBrickTypeがTailで無いことを判定する
                             let d = interval model1
                             in 
                                unit (\ab ar -> ASTxy (x, y) (ASTne n ab ar))
                                |> andMap (recurAttachAST d f u (x, y + d) b) 
                                |> andMap (recurAttachAST d f u (x + d, y) r)
                         else unit (ASTxy (x, y) (ASTne n b r)) --追加、根のBrickTypeがTailの場合何も変更を加えない処理を行う
                         )
                      )
                      model1.getASTRoots
                newModel = 
                    { model1 | getASTRoots = newRoots }

                (pickX, pickY) = event.pagePos
            in
                -- 接木が成功したことを確認したときだけリストから削除する または、削除の対象
                if isChanged == Changed || pickX < 400 then
                    newModel |> removeAST (ASTxy xy (ASTne node bottom right))
                else
                    rePos (ASTxy xy (ASTne node bottom right)) False newModel

-- ブロックの座標確認用関数
-- 座標の照合のために、let式で対象のブロック情報を一度削除する
rePos : ASTxy Node -> Bool -> Model -> Model
rePos (ASTxy xy (ASTne node bottom right)) new model =
    if not model.getDnDInfo.getOnDnD && not new then model
    else
        let model2 = if not new then removeAST (ASTxy xy (ASTne node bottom right)) model
                     else model
            (pickX, pickY) = xy
            x0 = if pickX < 400 then 400 else pickX
            y0 = if pickY < 60 then 60 else pickY
            xy0 = (x0, y0)
        in  addASTxy
              (ASTxy
                (fixPos xy0 model.nowTabE (List.map pickPos model2.getASTRoots))
                (ASTne {node | getBrickTab = model.nowTabE} bottom right)
              )
              model2

-- ブロック情報から座標とタブ番号のみ取り出す関数
pickPos : ASTxy Node -> (Position, Int)
pickPos (ASTxy xy (ASTne node bottom right)) = (xy, node.getBrickTab)

-- ブロック座標の照合の処理をする関数
-- 対象ブロックと座標が近いブロックが存在する場合は遠ざかる
fixPos : Position -> Int -> List (Position, Int) -> Position
fixPos (x, y) t listPT =
    let
        xy : List Position
        xy = List.repeat (List.length listxy) (x, y)

        tab : List Int
        tab = List.repeat (List.length listTab) t

        (listxy, listTab) = List.unzip listPT
        
        fixResult : Bool
        fixResult = List.member True (List.map4 comparePos xy tab listxy listTab)
    in
        if fixResult then fixPos (x + 35, y + 41) t listPT
        else (x, y)

-- ブロック照合判定用関数
-- insideBrickとほぼ同じ処理
comparePos : Position -> Int -> Position -> Int -> Bool
comparePos (x0, y0) t0 (x, y) t = 
    y0 - 35 <= y && y <= y0 + 35 &&
    x0 - 35 <= x && x <= x0 + 35 &&
    t0 == t

-- 入力した引数の格納
-- この関数では根の座標を参照し、対象の構文木を検索する
inputValuexy : Position -> String -> String -> Model -> Model
inputValuexy xy argCode value model =
    { model | getASTRoots = 
        model.getASTRoots 
        |> List.map(\(ASTxy p (ASTne n b r)) -> 
            if p == xy && n.getBrickTab == model.nowTabE then
                if String.isEmpty argCode then
                    ASTxy p (ASTne {n | getBrickArgument = {disp = n.getBrickArgument.disp, value = value} } b r)
                else
                    ASTxy p (ASTne n (inputValue argCode "b" value b) (inputValue argCode "r" value r))
            else
                ASTxy p (ASTne n b r)
        )
    }

-- 入力した引数の格納
-- この関数では葉の座標を参照し、対象のブロックに文字列を格納する
inputValue : String -> String -> String -> AST Node -> AST Node
inputValue argCode checkCode value ast =
    case ast of
        Nil ->
            Nil
        AST n b r ->
            if argCode == checkCode then
                AST { n | getBrickArgument = {disp = n.getBrickArgument.disp, value = value} } b r
            else
                AST n (inputValue argCode (checkCode ++ "b") value b) (inputValue argCode (checkCode ++ "r") value r)



-- VIEW

view : Model -> Html Msg
view model =
    div [ style "position" "relative"
            , style "width" "100vh"
            , style "height" "100vh"
            , on "mousemove" (\event -> MsgMoveUs event)
        ]
        [ div [ style "position" "absolute"
              , style "top" "0px"
              , style "left" "0px"
              , style "width" "400px"
              , style "height" "100vh"
              , style "background" "#f0f0f0"
              ] [] --[div [] [Html.text "test1"], div [] [Html.text "test2"]]
        , button [ style "position" "absolute"
                 , style "top" "0px"
                 , style "left" "0px"
                 , onClick MsgLoadFile
                 ] [ Html.text "プログラム読み込み" ]
        , button [ style "position" "absolute"
                 , style "top" "26px"
                 , style "left" "0px"
                 , onClick MsgSaveFile
                 ] [ Html.text "プログラム保存" ]
        , button [ style "position" "absolute"
                 , style "top" "52px"
                 , style "left" "0px"
                 , onClick MsgCheckReset
                 ] [ Html.text "プログラム削除" ]
        , button [ style "position" "absolute"
                 , style "top" "0px"
                 , style "left" "400px"
                 , onClick MsgEncoding
                 ] [ Html.text "プログラム送信" ]
        , setTabP
          |> List.indexedMap (\index tab -> (String.fromInt index, viewTab tab model.nowTabP True))
          |> Keyed.node "div" []
        , setTabE
          |> List.indexedMap (\index tab -> (String.fromInt index, viewTab tab model.nowTabE False))
          |> Keyed.node "div" []
        , setPallet
          |> List.indexedMap (\index astxy -> (String.fromInt index, viewASTxy model True astxy)) 
          |> Keyed.node "div" []
        , model.getASTRoots
          |> List.indexedMap (\index astxy -> (String.fromInt index, viewASTxy model False astxy)) 
          |> Keyed.node "div" []
        ]

-- 根のブロックの描画
viewASTxy : Model -> Bool -> ASTxy Node -> Html Msg
viewASTxy model te (ASTxy (x1, y1) (ASTne n b r)) = 
    let
      (x, y) = if (x1,y1) == model.getDnDInfo.getBrickXY then model.getDnDInfo.getMoveXY
               else (x1,y1)
    in
    if not te && not (model.nowTabE == n.getBrickTab) then Keyed.node "div" [] []
    else if te && not (model.nowTabP == n.getBrickTab) then Keyed.node "div" [] []
    else Keyed.node "div"
    [ style "position" "absolute"
    , style "top"  (String.fromFloat y ++ "px")
    , style "left" (String.fromFloat x ++ "px")
    -- MsgLetMeRootは部分木に対してしか意味をなさないのでここで根にはセットしない
    -- 代わりにmousedownに対してはMsgStartDnDを単独でセット
    , on "mouseup"   (\event -> MsgAttachMe (ASTxy (x, y) (ASTne n b r)) (x1,y1) event)
    , on "contextmenu" (\event -> MsgCloneUs (ASTne n b r) event)
    , on "mousedown" (\event -> MsgStartDnD (ASTxy (x, y) (ASTne n b r)) event) 
    ] 
    [ --("N", brickSvg model.getBrickSize)　変更箇所
    ("N", case n.getBrickType of
                    BasicBrick ->
                        basicBrickSvg model.getBrickSize n.getBrickCommand n.getBrickArgument "" (x,y)
                    EntryBrick ->
                        entryBrickSvg model.getBrickSize n.getBrickCommand n.getBrickArgument "" (x,y)
                    TailBrick ->
                        tailBrickSvg model.getBrickSize n.getBrickCommand n.getBrickArgument "" (x,y)
                    CaseBrick ->
                        caseBrickSvg model.getBrickSize n.getBrickCommand n.getBrickArgument "" (x,y)
        ) -- 実際のブロックの描画はbrickSvgで
    , ("R", lazy5 viewAST model.getBrickSize True  r "r" (x,y))
    , ("B", lazy5 viewAST model.getBrickSize False b "b" (x,y))
    ]  

-- 根以外の描画
viewAST: Magnitude -> Bool -> AST Node -> String -> Position -> Html Msg
viewAST size isRight ast argCode xy = 
    case ast of
        Nil ->
            div [] [] 
        AST node b r ->
            Keyed.node "div"
                [ style "position" "absolute"
                , style "top"  <| (if isRight then "0" else String.fromFloat (size * 0.9)) ++ "px"
                , style "left" <| (if isRight then String.fromFloat (size * 0.9) else "0") ++ "px"
                , on "contextmenu" (\event -> MsgCloneUs   (ASTne node b r) event)
                , on "mousedown"   (\event -> MsgLetMeRoot (ASTne node b r) xy event)
                ]
                [ --("N", brickSvg size) 変更箇所
                ("N", case node.getBrickType of
                            BasicBrick ->
                                basicBrickSvg size node.getBrickCommand node.getBrickArgument argCode xy
                            EntryBrick ->
                                entryBrickSvg size node.getBrickCommand node.getBrickArgument argCode xy
                            TailBrick ->
                                tailBrickSvg size node.getBrickCommand node.getBrickArgument argCode xy
                            CaseBrick ->
                                caseBrickSvg size node.getBrickCommand node.getBrickArgument argCode xy
                ) -- 実際のブロックの描画はbrickSvgで
                , ("R", lazy5 viewAST size True  r (argCode ++ "r") xy)
                , ("B", lazy5 viewAST size False b (argCode ++ "b") xy)
                ]                        

-- ブリック要素は入れ子の木になっているので伝搬を止めないと複数のブリックが
-- イベントを複数ひろってしまうことに注意
on : String -> (Mouse.Event -> msg) -> Html.Attribute msg
on eventName =
    { stopPropagation = True, preventDefault = if eventName == "contextmenu" || eventName == "mousemove" then True else False }
        |> Mouse.onWithOptions eventName

-- 実際に各々のブロックを描く関数
basicBrickSvg : Float ->  BrickCommand -> Argument -> String -> Position -> Html Msg
basicBrickSvg size command argument argCode xy = div [] 
  [ Svg.svg 
    [ width  <| String.fromFloat size
    , height <| String.fromFloat size 
    , viewBox "166 70 336 336"
    ]
    [ Svg.path
        [ d "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
        , stroke "gray"
        , fill "yellow"
        , strokeWidth "6"
        ] []
    , Svg.svg [x "166", y"70", viewBox "0 0 135 50"] [
        Svg.text_ [x "15" , y "0" , fill "black"] [Svg.text (viewCommand command)]
      , Svg.text_ [x "20" , y "25", fill "black"] [Svg.text (argument.disp)]
      ]
    ]
  , if not (String.isEmpty argument.disp) then
      --引数を実装するなら描画する
      input [ placeholder "引数" --未入力時の表記
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "50px"
            , Html.Attributes.style "left" "20px"
            , Html.Attributes.style "width" "40px"
            , value argument.value --入力されている値
            , on "mouseup" (\event -> Dummy event) --バブリングを無効にする
            , on "mousedown" (\event -> Dummy event) --バブリングを無効にする
            --inputイベント定義
            , stopPropagationOn "input" <| Decode.map alwaysStop (Decode.map (MsgInputChanged xy argCode) targetValue)
            ] []
    else div [] []
  ]

alwaysStop : a -> (a, Bool)
alwaysStop x =
    (x, True)

entryBrickSvg size command argument argCode xy = div []
  [ Svg.svg 
    [ width  <| String.fromFloat size
    , height <| String.fromFloat size 
    , viewBox "166 70 336 336"
    ]
    [ Svg.path
        [ d "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100  L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632  L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
        , stroke "gray"
        , fill "skyblue"
        , strokeWidth "6"
        ] []
    , Svg.svg [x "166", y"70", viewBox "0 0 135 50"]
      [Svg.text_ [x "15" , y "0" , fill "black"] [Svg.text (viewCommand command)]
      , Svg.text_ [x "20" , y "25", fill "black"] [Svg.text (argument.disp)]
      ]
    ]
  , if not (String.isEmpty argument.disp) then
      --引数を実装するなら描画する
      input [ placeholder "引数" --未入力時の表記
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "50px"
            , Html.Attributes.style "left" "50px"
            , Html.Attributes.style "width" "40px"
            , value argument.value --入力されている値
            , on "mouseup" (\event -> Dummy event) --バブリングを無効にする
            , on "mousedown" (\event -> Dummy event) --バブリングを無効にする
            --inputイベント定義
            , stopPropagationOn "input" <| Decode.map alwaysStop (Decode.map (MsgInputChanged xy argCode) targetValue)
            ] []
    else div [] []
  ]

tailBrickSvg size command argument argCode xy = div []
  [ Svg.svg 
    [ width  <| String.fromFloat size
    , height <| String.fromFloat size 
    , viewBox "166 70 336 336"
    ]
    [ Svg.path
        [ d "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 Z"
        , stroke "gray"
        , fill "pink"
        , strokeWidth "6"
        ] []
    , Svg.svg [x "166", y"70", viewBox "0 0 135 50"]
      [Svg.text_ [x "15" , y "0" , fill "black"] [Svg.text (viewCommand command)]
      , Svg.text_ [x "20" , y "25", fill "black"] [Svg.text (argument.disp)]
      ]
    ]
  , if not (String.isEmpty argument.disp) then
      --引数を実装するなら描画する
      input [ placeholder "引数" --未入力時の表記
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "50px"
            , Html.Attributes.style "left" "50px"
            , Html.Attributes.style "width" "40px"
            , value argument.value --入力されている値
            , on "mouseup" (\event -> Dummy event) --バブリングを無効にする
            , on "mousedown" (\event -> Dummy event) --バブリングを無効にする
            --inputイベント定義
            , stopPropagationOn "input" <| Decode.map alwaysStop (Decode.map (MsgInputChanged xy argCode) targetValue)
            ] []
    else div [] []
  ]
    
caseBrickSvg size command argument argCode xy = div []
  [ Svg.svg 
   [ width  <| String.fromFloat size
    , height <| String.fromFloat size 
    , viewBox "166 70 336 336"
    ]
    [ Svg.path
        [ d "M 500 220.06632 L 500 110 C 500 104.47715 495.52285 100 490 100 L 379.9337 100 C 379.46895 93.00195 376.5621 86.13573 371.2132 80.78682 C 359.49746 69.07104 340.50254 69.07104 328.78682 80.78682 C 323.4379 86.13573 320.53105 93.00195 320.0663 100 L 320.0663 100 L 210 100 C 204.47715 100 200 104.47715 200 110 L 200 220.06632 C 191.6702 219.51314 183.15363 222.41997 176.78682 228.78682 C 165.07104 240.50254 165.07104 259.49746 176.78682 271.21318 C 183.15363 277.58003 191.6702 280.48686 200 279.9337 L 200 390 C 200 395.52285 204.47715 400 210 400 L 320.0663 400 C 320.53105 393.00195 323.4379 386.13573 328.78682 380.7868 C 340.50254 369.07104 359.49746 369.07104 371.2132 380.7868 C 376.5621 386.13573 379.46895 393.00195 379.9337 400 L 490 400 C 495.52285 400 500 395.52285 500 390 L 500 279.9337 C 491.6702 280.48686 483.15363 277.58003 476.7868 271.21318 C 465.07104 259.49746 465.07104 240.50254 476.7868 228.78682 C 483.15363 222.41997 491.6702 219.51314 500 220.06632 Z"
        , stroke "gray"
        , fill "limegreen"
        , strokeWidth "6"
        ] []
    , Svg.svg [x "166", y"70", viewBox "0 0 135 50"]
      [Svg.text_ [x "15" , y "0" , fill "black"] [Svg.text (viewCommand command)]
      , Svg.text_ [x "70" , y "25", fill "black"] [Svg.text (argument.disp)]
      ]
    ]
  , if not (String.isEmpty argument.disp) then
      --引数を実装するなら描画する
      input [ placeholder "引数" --未入力時の表記
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "50px"
            , Html.Attributes.style "left" "20px"
            , Html.Attributes.style "width" "40px"
            , value argument.value --入力されている値
            , on "mouseup" (\event -> Dummy event) --バブリングを無効にする
            , on "mousedown" (\event -> Dummy event) --バブリングを無効にする
            --inputイベント定義
            , stopPropagationOn "input" <| Decode.map alwaysStop (Decode.map (MsgInputChanged xy argCode) targetValue)
            ] []
    else div [] []
  ] 

-- BrickCommandを対応した文字列に変換
viewCommand : BrickCommand -> String
viewCommand command =
    case command of
      CommandNOP -> "動作開始"
      CommandMove -> "前進する"
      CommandTurnRight -> "右を向く"
      CommandTurnLeft -> "左を向く"
      CommandTurnBack -> "後を向く"
      CommandFuncStart -> "関数nを開始"
      CommandFuncStop -> "関数nへ移動"
      CommandIfS -> "前に進めるなら"
      CommandIfR -> "右に進めるなら"
      CommandIfL -> "左に進めるなら"
      CommandIfB -> "後ろ進めるなら"
      CommandNone -> "なにもしない"
      CommandReboot -> "動作開始へ移動"
      --_ -> "error"

-- 描画するタブの情報
type alias TabInfo
    = { number : Int
      , text : String
      , color : String
      }

-- パレットに描画するタブ
setTabP : List TabInfo
setTabP =
    [ { number = 0, text = "行動", color = "#ffff00" }
    , { number = 1, text = "判断", color = "#00ff00" }
    , { number = 2, text = "関数", color = "#ffBBBB" }
    --, { number = 3, text = "変数", color = "#ff0000" }
    ]

--エディタに描画するタブ
setTabE : List TabInfo
setTabE =
    [ { number = 0, text = "開始", color = "#00ffff" }
    , { number = 1, text = "関数1", color = "#ffff00" }
    , { number = 2, text = "関数2", color = "#ff00ff" }
    , { number = 3, text = "関数3", color = "#00ffff" }
    , { number = 4, text = "関数4", color = "#ffff00" }
    , { number = 5, text = "関数5", color = "#ff00ff" }
    ]

-- タブの描画
-- ただのdiv要素
viewTab : TabInfo -> Int -> Bool -> Html Msg
viewTab tab now te =
    div [ style "position" "absolute"
        , style "top" <| String.fromInt (if te then tab.number * 53 + 100 else 0) ++ "px"
        , style "left" <| String.fromInt (if te then 0 else tab.number * 53 + 550) ++ "px"
        , style "width" "50px"
        , style "height" "50px"
        , style "background" <| if tab.number == now then tab.color else "#c0c0c0"
        , on "click" (\event -> MsgChangeTab tab.number te)
        ] [ Html.text tab.text ]




-- デコード処理
-- 抽象構文木単体を変換
decodeASTxy : Decode.Value -> ASTxy Node
decodeASTxy ast =
    let
        posX = 
            case Decode.decodeValue (Decode.at ["position", "x"] Decode.float) ast of
                Ok x -> x
                Err _ -> 0
        posY = 
            case Decode.decodeValue (Decode.at ["position", "y"] Decode.float) ast of
                Ok y -> y
                Err _ -> 0
        node =
            case Decode.decodeValue (Decode.at ["ASTne", "node"] Decode.value) ast of
                Ok n -> decodeNode n
                Err _ ->
                    { getBrickType = BasicBrick
                    , getBrickCommand = CommandNone
                    , getBrickArgument = {disp = "", value = ""}
                    , getBrickTab = 0
                    }
        bottom =
            case Decode.decodeValue (Decode.at ["ASTne", "bottom"] Decode.value) ast of
                Ok b -> decodeAST b
                Err _ -> Nil 
        right =
            case Decode.decodeValue (Decode.at ["ASTne", "right"] Decode.value) ast of
                Ok r -> decodeAST r
                Err _ -> Nil     
    in
    ASTxy
        (posX, posY)
        (ASTne node bottom right)

-- デコード用　ブロック情報を変換
decodeNode : Decode.Value -> Node
decodeNode node =
    { getBrickType =
        case Decode.decodeValue (Decode.at ["getBrickType"] Decode.string) node of
                Ok ty -> decodeBrickType ty
                Err _ -> BasicBrick
    , getBrickCommand =
        case Decode.decodeValue (Decode.at ["getBrickCommand"] Decode.string) node of
                Ok c -> decodeBrickCommand c
                Err _ -> CommandNone
    , getBrickArgument =
        { disp =
            case Decode.decodeValue (Decode.at ["getBrickArgument","disp"] Decode.string) node of
                Ok d -> d
                Err _ -> ""
        , value =
            case Decode.decodeValue (Decode.at ["getBrickArgument","value"] Decode.string) node of
                Ok v -> v
                Err _ -> ""
        }
    , getBrickTab =
        case Decode.decodeValue (Decode.at ["getBrickTab"] Decode.int) node of
                Ok ta -> ta
                Err _ -> 0
    }

-- デコード用　文字列をBrickTypeに変換
decodeBrickType : String -> BrickType
decodeBrickType t =
    case t of
      "EntryBrick" -> EntryBrick
      "BasicBrick" -> BasicBrick
      "TailBrick" -> TailBrick
      "CaseBrick" -> CaseBrick
      _ -> BasicBrick

-- デコード用　文字列をBrickCommandに変換
decodeBrickCommand : String -> BrickCommand
decodeBrickCommand c =
    case c of
      "CommandNOP" -> CommandNOP
      "CommandMove" -> CommandMove
      "CommandTurnRight" -> CommandTurnRight
      "CommandTurnLeft" -> CommandTurnLeft
      "CommandTurnBack" -> CommandTurnBack
      "CommandFuncStart" -> CommandFuncStart
      "CommandFuncStop" -> CommandFuncStop
      "CommandIfS" -> CommandIfS
      "CommandIfR" -> CommandIfR
      "CommandIfL" -> CommandIfL
      "CommandIfB" -> CommandIfB
      "CommandNone" -> CommandNone
      "CommandReboot" -> CommandReboot
      _ -> CommandNone

-- デコード用　ブロック単体の変換
decodeAST : Decode.Value -> AST Node
decodeAST ast =
    let
        (check, node) =
            case Decode.decodeValue (Decode.at ["node"] Decode.value) ast of
                Ok n -> (True, decodeNode n)
                Err _ -> (False,
                    { getBrickType = BasicBrick
                    , getBrickCommand = CommandNone
                    , getBrickArgument = {disp = "", value = ""}
                    , getBrickTab = 0
                    })
        bottom =
            case Decode.decodeValue (Decode.at ["bottom"] Decode.value) ast of
                Ok b -> decodeAST b
                Err _ -> Nil 
        right =
            case Decode.decodeValue (Decode.at ["right"] Decode.value) ast of
                Ok r -> decodeAST r
                Err _ -> Nil
    in
        if check then
            AST node bottom right
        else
            Nil



subscriptions : Model -> Sub Msg
subscriptions model = receiveMes MsgGetMessage