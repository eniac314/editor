module Editor exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words)
import Http
import Json.Decode as Json
import Task
import HtmlZipper exposing (HTML, htmlToString)
import ElmParser exposing ( interpret
                          , renderer
                          )

main =
    App.program { init   = (Model "" (Err "nothing yet") (text ""), Cmd.none) 
                , update = update
                , view   = view
                , subscriptions = subscriptions
                }

type alias Model = 
  { rawString : String
  , parsedData : Result String HTML
  , toRender : Html Msg
  }

-- UPDATE

type Msg = Store String
         | Parse
         | Render

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    Store s -> ({ model | rawString = s}, Cmd.none)
    Parse   -> 
       ({ model |
          parsedData = interpret (.rawString model)
        }, Cmd.none)
    Render  -> ({ model | 
                  toRender = (renderer (.parsedData model))
                }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
    div []
        [ textarea [ 
                     onInput Store
                   , rows 15
                   , cols 45
                   , inputStyle
                   ]
                   [text testinput]
        , br [] []
        , button [onClick Parse] [ text "Parse"]
        , button [onClick Render] [ text "Render"]
        , br [] []
        , text (toString (.parsedData model))
        , br [] []
        , text (case (.parsedData model) of 
                        Err s -> s
                        Ok r  -> htmlToString r) 
            
              
        , br [] []
        , (.toRender model )
        ]

inputStyle = 
  style [("font-family","monospace")
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