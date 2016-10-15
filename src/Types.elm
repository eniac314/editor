module Types exposing (..)

import Html exposing (..)
import Data.Integer exposing (Integer)
import Window as Win
import HtmlZipper exposing ( HTML
                           , HtmlZipper
                           , Path
                           , Tag
                           , Tree (..)
                           , ZipTree (..)
                           )

type AppPos = MainMenu | Editor | FileIO | Renderer



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
  , nblines : Int
  }

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
         | ChangeUrl String
         | Failure