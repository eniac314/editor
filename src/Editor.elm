module Editor exposing (..)

import Html exposing (..)
import Task exposing (perform)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words)
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

{ id, class, classList } =
    Html.CssHelpers.withNamespace "editor"


main =
    Navigation.program urlParser
                { init   = (\_ -> (init testinput, initWinSize)) 
                , update = update
                , urlUpdate = urlUpdate
                , view   = view
                , subscriptions = subscriptions
                }


initWinSize = 
  perform (\_ -> Failure)
          (\s -> WinSize s)
          Win.size

urlParser : Navigation.Parser (Result String AppPos)
urlParser = Navigation.makeParser 
  (\s -> 
    let validUrlMap = 
      Dict.fromList [("main",MainMenu)
                    ,("editor",Editor)
                    ,("fileIO",FileIO)
                    ] 
    in case get (.hash s) validUrlMap of
        Nothing -> Err "invalid url"
        Just ap -> Ok ap)

type AppPos = MainMenu | Editor | FileIO

type alias Model = 
  { position : AppPos
  , rawString : String
  , procString : Maybe String
  , parsedData : Result String (HTML,Integer)
  , currPath : Path 
  , page : Maybe HtmlZipper
  , toRender : Html Msg
  , nextId : Integer
  , debug : Bool
  , winSize : Maybe Win.Size
  }

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

type Msg = Store String
         | Parse
         | Render
         | Up 
         | Down
         | Left
         | Right
         | GoTo Path
         | Debug
         | WinSize Win.Size
         | Failure

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    Store s -> { model | rawString = s} ! []
    Parse   -> parse model ! []
    Up -> move zipUp model ! []
    Down -> move zipDownFirst model ! []
    Left -> move zipLeft model ! []
    Right -> move zipRight model ! []
    GoTo path -> move (cd' path) model ! []
    Debug -> {model | debug = not (.debug model)} ! []
    Render  ->  { model | 
                  toRender = (renderer (.parsedData model))
                } ! []
    Failure -> model ! []
    WinSize s -> {model | winSize = Just s} ! []


urlUpdate : Result String AppPos -> Model -> (Model, Cmd Msg)
urlUpdate _ model = (model, Cmd.none)

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
        --case newPage of 
        -- Nothing -> (.toRender model)
        -- Just np -> renderer (extract np)

  in { model | rawString = newRstring
             , procString = newProcString
             , parsedData = newParsedData
             , currPath = newPath
             , page = newPage
             , toRender = newRender
     }
     

--reset = Task.perform (\_ -> Reset) (\_ -> Reset) (succeed Reset)
-- VIEW


view : Model -> Html Msg
view model = 
    div [ id Editor]
         [ EditorCss.editorStyle
         , text (String.join "/" 
                   (List.reverse 
                      (List.map (\(t,p) -> toString t) (.currPath model))))
         , br  [] []
         , div [ class [ Pane ]
               , id LeftPane
               ] 
               [ Html.form 
                  []
                  [ textarea [ onInput Store
                             , rows 15
                             , cols 45
                             , id Prompt
                             ]
                             [ case (.procString model) of 
                                Nothing -> text (.rawString model)
                                Just s  -> text s
                             ]
                  , br [] []
                  , button [onClick Parse, type' "reset"] [ text "Parse"]
                  , button [onClick Render, type' "button"] [ text "Render"]
                  , button [onClick Debug, type' "button"] [ text "Debug"]
                  ]
              ]
        , div [ id RightPane
              , class [Pane]
              ]
              [ explorer model
              ]
        
        
        , div [ id "console"]
              [ case (.parsedData model) of 
                 Err s -> text s
                 Ok r  -> text "parsing complete" 
              ]
        , br [] []
        , text (toString (.winSize model))
        
        , br [] []
        , (.toRender model )
        , style' ".TgName:hover {fill: #8FBC8F; } 
                  .TgName{
                   cursor:pointer;
                  }
                 "
        ]

explorer : Model -> Html Msg
explorer model = 
  let
  page = .page model
  dbug = .debug model
  
  sizeExplorer = 
    case .winSize model of
      Nothing -> (560 ,300)
      Just {width, height} -> (width//2,300)
  
  explWindow tags =
    div [ id ExplWindow]
        [ tags ]
  tags = 
    case page of 
      Nothing -> span [] []
      Just zp -> render zp
    
  renderSvg (ZipTree (t ,ctx)) = 
    let
     
     colors = List.reverse ["ivory","khaki","lavender","lavenderblush"
              ,"lightcoral","lightgreen","lemonchiffon"
              ,"thistle","mediumspringgreen","lightskyblue"
              ]

     colorPicker xs = 
      case xs of
        [] -> (colorPicker colors) 
        (x::xs) -> (x,xs)
     
     renderSvgTag (xPos,yPos) c (Node tag xs) =
      let tn   = 
           case (.tagname tag) of 
            TagAttr.Text _ -> "Text"
            tn'    -> toString tn'
          pth  = .path tag
      in [rect [ fill c
               
               , x (toString xPos)
               , y (toString yPos)
               , Svg.Attributes.width "75"
               , Svg.Attributes.height "20"
               ]
               []
          , text' [fill "black"
                  , x (toString xPos)
                  , y (toString (yPos + 15))
                  , onClick (GoTo pth)
                  , Svg.Attributes.class "TgName"
                  ]
                  [Svg.text tn]
          ]
      
     render' (xPos,yPos) cs (Node tag xs) =
       let (c,cs') = colorPicker cs 
           t = renderSvgTag (xPos,yPos) c (Node tag xs)
           
           hori (xp,yp) xs = 
                case xs of 
                [] -> []
                (x::xs) -> 
                  let head = render' (xp, yp) cs' x
                      n = List.length head
                  in head :: hori (xp, yp + (n*15)) xs
        
        in t ++ (List.concat <| hori (xPos + 15, yPos+30) xs)

         
    in Svg.svg
        [ viewBox "0 0 500 4000"
        , Svg.Attributes.width (toString <| fst sizeExplorer)
        , Svg.Attributes.height (toString 4000) 
        ]
        (render' (10,10) colors t)    

  render (ZipTree (t ,ctx)) = 
    let
     spacer indent = 
       if indent == 0
       then ""
       else " " ++ (spacer (indent - 1))
     
     colors = List.reverse ["ivory","khaki","lavender","lavenderblush"
              ,"lightcoral","lightgreen","lemonchiffon"
              ,"thistle","mediumspringgreen","lightskyblue"
              ]

     colorPicker xs = 
      case xs of
        [] -> (colorPicker colors) 
        (x::xs) -> (x,xs)

     render' n cs (Node tag xs) =
      let tn   = 
           case (.tagname tag) of 
            TagAttr.Text _ -> "Text"
            tn'    -> toString tn'
          pth  = .path tag

          (c,cs') = colorPicker cs
          
      in p [ class []
           , style [("margin","0.1em")]
           , onClick (GoTo pth)
           ]
           ([ text (spacer n)
            , span [class [ExplTag]
            , style [  ("background-color",c) ]
            ] 
            [text tn]
            ] 
            ++ [span [classList [("Debug",dbug)]] [text (toString pth)]] ++ 
            (List.map (render' (n+3) cs') xs))

    in render' 0 colors t    


  in div [ id "explorer"
         , style [("width","95%")
                 , ("white-space","pre")
                 ]
         ]
         [ explWindow tags
         , button [onClick Up, type' "reset"] [ text "Left"]
         , button [onClick Down, type' "reset"] [ text "Right"]
         , button [onClick Left, type' "reset"] [ text "Up"]
         , button [onClick Right, type' "reset"] [ text "Down"]
         ]


  

style' : String -> Html msg
style' text =
    Html.node "style"
        [ property "textContent" <| Json.Encode.string text
        , property "type" <| Json.Encode.string "text/css"
        ]
        []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch [Win.resizes WinSize]


-- Test

testinput = 
  """ div [ class "main"]
          [ header [] [h1 [] [text "A great page"]]
          , body []
                 [form []
                       [ textarea [] [text "placeholder"] 
                       , button [] [text "press here!"]
                       ]
                  , a [href "http://www.google.com"]
                      [text "the answer to everything"]
                 , table []
                         [ th  [] [text "table header"]
                         , tr  [] [td [] [text "case 1"]
                                  ,td [] [text "case 2"]
                                  ,td [] [text "case 3"]
                                  ,td [] [text "case 4"]
                                  ]
                          , tr  [] [td [] [text "case 5"]
                                  ,td [] [text "case 6"]
                                  ,td [] [text "case 7"]
                                  ,td [] [text "case 8"]
                                  ]
                         ]
                 ]
          , footer [] [text "this is the end"]
          ]

  """
testinput2 = 
   """ textarea [] [text "hello"]

  """

testinput3 = 
   """ [ class "mainDiv" , id "toto" ]
   """

testinput4 = 
  """ div []
      [ h2 [] [text "first title"]
      ] 
  
  """