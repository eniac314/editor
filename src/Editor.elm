module Editor exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words)
import Http
import Json.Decode as Json
import Task exposing (succeed, perform)
import HtmlZipper exposing (HTML, htmlToString)
import ElmParser exposing ( interpret
                          , renderer
                          )

main =
    App.program { init   = (Model testinput Nothing (Err "nothing yet") (text ""), Cmd.none) 
                , update = update
                , view   = view
                , subscriptions = subscriptions
                }

type alias Model = 
  { rawString : String
  , procString : Maybe String 
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
        let pdata = interpret (.rawString model)
            prString = case pdata of 
                         Err s -> Nothing
                         Ok r  -> Just (htmlToString r)
        in
        ({ model |
           parsedData = pdata 
         , procString = prString
         }, Cmd.none)
    Render  -> ({ model | 
                  toRender = (renderer (.parsedData model))
                }, Cmd.none)


--reset = Task.perform (\_ -> Reset) (\_ -> Reset) (succeed Reset)
-- VIEW

view : Model -> Html Msg
view model = 
    div []
         [ Html.form []
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
         ]
        , button [onClick Render] [ text "Render"]
        
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