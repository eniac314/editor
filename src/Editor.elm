module Editor exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words)
import Http
import Json.Decode as Json
import Task
import BetterParser exposing (HTML
                             , parse
                             , Parser
                             , parseTag
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
          parsedData = 
            case (parse parseTag 
                   (words (.rawString model))) of 
                Err s -> Err s
                Ok (res,_) -> Ok res
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
                   ]
                   []
        , br [] []
        , button [onClick Parse] [ text "Parse"]
        , button [onClick Render] [ text "Render"]
        , br [] []
        , (.toRender model )
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


