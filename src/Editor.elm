module Editor exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words)
import Http
import Json.Decode as Json
import Task exposing (succeed, perform)
import Data.Integer exposing (fromInt, add, Integer)
import HtmlZipper exposing ( HTML
                           , HtmlZipper
                           , Path
                           , Tag
                           , htmlToString
                           , initZip
                           , updateFocus
                           , extractTree
                           , extractTag
                           , extractPath
                           , zipUp
                           , zipDownFirst
                           , zipLeft
                           , zipRight
                           )
import ElmParser exposing ( interpret
                          , renderer
                          )

main =
    App.program { init   = (init testinput, Cmd.none) 
                , update = update
                , view   = view
                , subscriptions = subscriptions
                }

type alias Model = 
  { rawString : String
  , procString : Maybe String
  , parsedData : Result String (HTML,Integer)
  , currPath : Path 
  , page : Maybe HtmlZipper
  , toRender : Html Msg
  , nextId : Integer
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
  Model initInput
        Nothing
        pdata
        initPath
        initPage        
        (renderer pdata)
        nextId
        

-- UPDATE

type Msg = Store String
         | Parse
         | Render
         | Up 
         | Down
         | Left
         | Right

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    Store s -> ({ model | rawString = s}, Cmd.none)
    Parse   -> (parse model, Cmd.none)
    Up -> (move zipUp model,Cmd.none)
    Down -> (move zipDownFirst model, Cmd.none)
    Left -> (move zipLeft model, Cmd.none)
    Right -> (move zipRight model, Cmd.none)

    Render  -> ({ model | 
                  toRender = (renderer (.parsedData model))
                }, Cmd.none)


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
              Just p  -> Just (updateFocus r p)
      nextId =
       case pdata of 
         Err s -> .nextId model
         Ok (_,n) -> add n (fromInt 1)

  in { model | procString = prString
             , parsedData = pdata
             , page = newPage
             , nextId = nextId
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

  in Model newRstring
           newProcString
           newParsedData
           newPath
           newPage        
           newRender
           (.nextId model) 

--reset = Task.perform (\_ -> Reset) (\_ -> Reset) (succeed Reset)
-- VIEW

view : Model -> Html Msg
view model = 
    div []
         [ text (String.join "/" 
                   (List.reverse 
                      (List.map toString (.currPath model))))
         , Html.form []
            [ textarea [ onInput Store
                       , rows 15
                       , cols 45
                       , inputStyle
                       ]
                       [ case (.procString model) of 
                           Nothing -> text (.rawString model)
                           Just s  -> text s
                       ]
            , br [] []
            , button [onClick Parse, type' "reset"] [ text "Parse"]
            , button [onClick Render, type' "button"] [ text "Render"]
            , button [onClick Up, type' "reset"] [ text "Up"]
            , button [onClick Down, type' "reset"] [ text "Down"]
            , button [onClick Left, type' "reset"] [ text "Left"]
            , button [onClick Right, type' "reset"] [ text "Right"]
         ]
        
        
        , br [] []
        , text (toString (.parsedData model))
        
        , br [] []
        , (.toRender model )
        ]

inputStyle = 
  style [("font-family","Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace")
        ,("width","40%")
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- Test

testinput = 
  """ div [ class "mainDiv" ]
          [ p [ style [ ( "color" , "red" ) ] ] [ text "this is a test" 
                  , p [ ] [ ]
                  ]
          , p [ ] [ h2 [ id "very important" , style [ ( "color" , "blue" ) ] ] [ text "big title" ] ]
          ]

  """
testinput2 = 
   """ div [ class "mainDiv" , id "toto" ]
           [ text "hello!" ]

  """

testinput3 = 
   """ [ class "mainDiv" , id "toto" ]
   """

testinput4 = 
  """ div []
      [ h2 [] [text "first title"]
      ] 
  
  """