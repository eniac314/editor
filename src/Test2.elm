import Dict exposing (Dict)
import Html exposing (Html, Attribute, a, div, hr, input, span ,text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Navigation
import String
import Task
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)

main = 
  Navigation.program (Navigation.makeParser hashParser)
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }

toHash : Page -> String 
toHash page = 
  case page of
    Home ->
      "#home"
    Blog id ->
      "#blog/" ++ toString id
    Search query ->
      "#search/" ++ query

hashParser : Navigation.Location -> Result String Page
hashParser location = 
  UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)

type Page = Home | Blog Int | Search String

pageParser : Parser (Page -> a) a
pageParser = 
  oneOf 
    [ format Home (s "home")
    ]