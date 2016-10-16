module Editor exposing (..)

import Html exposing (..)
import Task exposing (perform)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words, lines, join)
import Http
import Dict exposing (fromList, toList ,get)
import Json.Decode as Json
import Json.Encode exposing (string)
import Task exposing (succeed, perform)
import Data.Integer exposing (fromInt, add, Integer)
import Html.CssHelpers
import EditorCss exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import Types exposing (..)
import HtmlZipper exposing ( HTML
                           , HtmlZipper
                           , Path
                           , Tag
                           , Tree (..)
                           , ZipTree (..)
                           , htmlToString
                           , initZip
                           , updateTag
                           , extractTree
                           , extractTag
                           , extractPath
                           , zipUp
                           , zipDownFirst
                           , zipLeft
                           , zipRight
                           , cd'
                           , root
                           )
import TagAttr exposing (TagName)
import ElmParser exposing ( interpret
                          , renderer
                          )
import Svg exposing (svg, rect, text')
import Svg.Attributes exposing (width, height,viewBox, fill, x, y, class)
import Window as Win
import EditorView exposing (..)
import Pad exposing (..)
import Keyboard exposing (..)
import CssParser exposing (..)

{ id, class, classList } =
    Html.CssHelpers.withNamespace "editor"


main =
    Navigation.program urlParser
                { init   = init'
                , update = update
                , urlUpdate = urlUpdate
                , view   = view
                , subscriptions = subscriptions
                }


init' result = 
  let (m,cmd) = urlUpdate result (init testinput)
  in (m, Cmd.batch [ cmd
                   , initWinSize
                   , modifyUrl "#editor"])

initWinSize = 
  perform (\_ -> Failure)
          (\s -> WinSize s)
          Win.size

urlParser : Navigation.Parser (Result String AppPos)
urlParser = Navigation.makeParser 
  (\s -> 
    let validUrlMap = 
      Dict.fromList [("#mainmenu",MainMenu)
                    ,("#editor",Editor)
                    ,("#fileIO",FileIO)
                    ,("#renderer",Renderer)
                    ] 
    in case get (.hash s) validUrlMap of
        Nothing -> Err ("invalid url: " ++ (toString s))
        Just ap -> Ok ap)

urlUpdate : Result String AppPos -> Model -> (Model, Cmd Msg)
urlUpdate res model = 
  case res of 
    Err s -> ({model | rawString = s}, Cmd.none)
    Ok ap -> { model | position = ap } ! []



init initInput = 
  let pdata = interpret initInput (fromInt 0)
      initPage = 
        case pdata of 
          Err s -> Nothing
          Ok  (t,n) -> Just (initZip t)
      initPath = 
        case initPage of 
          Nothing -> []
          Just ip -> extractPath ip
      nextId = 
        case pdata of 
          Err s -> fromInt 0
          Ok (r,n) -> (add n (fromInt 1)) 
  in
  Model Editor
        initInput
        Nothing
        pdata
        initPath
        initPage        
        (renderer pdata)
        nextId
        True
        Nothing

-- UPDATE



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    Store s -> { model | rawString = s } ! []
    Parse   -> parse model ! []
    Up -> move zipUp model ! []
    Down -> move zipDownFirst model ! []
    Left -> move zipLeft model ! []
    Right -> move zipRight model ! []
    GoTo path -> move (cd' path) model ! []
    Debug -> {model | debug = not (.debug model)} ! []
    Failure -> model ! []
    WinSize s -> {model | winSize = Just s} ! []
    ChangeUrl s -> model ! [newUrl s]
    SwapEditorRender -> model ! [swapEditorRender model]


swapEditorRender model = 
  case .position model of
    Editor -> 
      perform (\_ -> Failure)
              (\url -> ChangeUrl url)
              (succeed "#renderer")
    Renderer -> 
      perform (\_ -> Failure)
              (\url -> ChangeUrl url)
              (succeed "#editor")
    _ -> Cmd.none

parse model = 
  let pdata = interpret (.rawString model) (.nextId model)
      prString = case pdata of 
                   Err s -> Nothing
                   Ok (r,n)  -> Just (htmlToString r)
      newPage = 
        case pdata of 
          Err s -> (.page model)
          Ok  (r,_) -> 
            case (.page model) of 
              Nothing -> Just (initZip r)
              Just p  -> Just (updateTag r p)
      nextId =
       case pdata of 
         Err s -> .nextId model
         Ok (_,n) -> add n (fromInt 1)

      currPath =
        case newPage of  
          Nothing -> .currPath model
          Just p -> extractPath p

  in { model | procString = prString
             , parsedData = pdata
             , page = newPage
             , nextId = nextId
             , currPath = currPath
             , toRender = renderer pdata
     } 

move : (HtmlZipper -> Maybe HtmlZipper) -> Model -> Model
move f model = 
  let newPage = 
       case (.page model) of 
         Nothing -> Nothing
         Just  p -> 
          case (f p) of 
            Nothing -> Just p
            Just np -> Just np

      newRstring = 
        case newPage of 
         Nothing -> (.rawString model)
         Just np -> htmlToString (extractTree np)
      
      newProcString = Just newRstring
      
      newParsedData = 
        case newPage of 
         Nothing -> Err "wrong Html tree"
         Just np -> Ok ((extractTree np),.nextId model)

      newPath    = 
        case newPage of 
         Nothing -> (.currPath model)
         Just np -> (extractPath np)

      newRender  = renderer newParsedData

  in { model | rawString = newRstring
             , procString = newProcString
             , parsedData = newParsedData
             , currPath = newPath
             , page = newPage
             , toRender = newRender
     }
     


-- VIEW


view : Model -> Html Msg
view model = 
  div [ id Container
      , setHeight (.winSize model)
      ]
      (
      [ EditorCss.editorStyle
      , renderMenu model
      ] ++
      ( case (.position model) of 
          MainMenu -> renderMainMenu model
          Editor -> renderEditor model
          FileIO -> renderFileIO model
          Renderer -> renderRenderer model
      ))

    
         



renderMainMenu model = []
renderFileIO model = []
renderRenderer model = 
  [ div [id RendererId] 
        [.toRender model]
  ]

renderMenu : Model -> Html Msg
renderMenu model = 
  div [ id Menu
      ]
      [ a [classList [("CurrentPos",(.position model == MainMenu))]
          , onClick (ChangeUrl "#mainmenu")
          ]
          [text "Main Menu"]
      , a [classList [("CurrentPos",(.position model == Editor))]
          , onClick (ChangeUrl "#editor")
          ]
          [text "Editor"]
      , a [classList [("CurrentPos",(.position model == FileIO))]
          , onClick (ChangeUrl "#fileIO")
          ]
          [text "Save/Load"]
      , a [classList [("CurrentPos",(.position model == Renderer))]
          , onClick (ChangeUrl "#renderer")
          ]
          [text "Html Preview"]
      ]




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch [Win.resizes WinSize, presses keyToMsg]


--Keyboard 
keyToMsg : (KeyCode -> Msg)
keyToMsg k = 
  let keys = 
        Dict.fromList 
          [
          --(37,Up)
          --,(39,Down)
          --,(38,Left)
          --,(40,Right)
          --,
           (112,Parse)
          ,(113,SwapEditorRender)
          ]
  in case get k keys of
    Nothing -> Failure
    Just msg -> msg




setHeight winSize = 
  case winSize of 
    Nothing -> style []
    Just {width, height} -> style [("height",toString height ++ "px")]