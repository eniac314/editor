module Types exposing (..)

import Html exposing (..)
--import Data.Integer exposing (Integer)
import Window as Win
import CssParser exposing (IndexedCss)
import HtmlZipper
    exposing
        ( HTML
        , HtmlZipper
        , Path
        , Tag
        , Tree(..)
        , ZipTree(..)
        )


type AppPos
    = MainMenu
    | Editor
    | FileIO
    | Renderer


type CssDict
    = CssClass
    | CssIds
    | CssPseudos
    | CssTags


type CssExplorerPos
    = Top
    | InDict ( String, List Int )


type alias CssExplorer =
    { currentPos : CssExplorerPos
    , currentDict : CssDict
    }


type alias Model =
    { position : AppPos
    , rawString : String
    , rawCssString : String
    , procString : Maybe String
    , procCssString : Maybe String
    , parsedData : Result String ( HTML, Int )
    , parsedCssData : Result String IndexedCss
    , currPath : Path
    , cssExplorer : CssExplorer
    , page : Maybe HtmlZipper
    , toRender : Html Msg
    , nextId : Int
    , debug : Bool
    , winSize : Maybe Win.Size
    }


type Msg
    = Store String
    | StoreCss String
    | Parse
    | ParseCss
    | Up
    | Down
    | Left
    | Right
    | GoTo Path
    | Debug
    | WinSize Win.Size
    | ChangeUrl String
    | NewUrl String
    | SwapEditorRender
    | ChangeDict CssDict
    | FilterCss ( String, List Int )
    | GoToCssTop
    | Failure
