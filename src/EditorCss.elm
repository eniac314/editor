module EditorCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Html.Attributes

type CssClasses
    = Pane | Debug | ExplTag


type CssIds
    = Editor | ExplWindow | Prompt
      



css =
    (stylesheet << namespace "editor")
    [ (.) Pane
        [ width (pct 50)
        , backgroundColor (rgb 155 155 155)
        , display inlineBlock
        ]
    , (.) Debug
        [ display none]
    , (.) ExplTag 
        [ padding (em 0.2)
        , borderStyle solid
        , borderColor (rgb 0 0 0)
        , margin (em 0.1)
        , cursor pointer
        ]               

    , (#) ExplWindow
        [ width (pct 100)
        , height (px 300)
        , overflow scroll
        ]
    , (#) Prompt 
        [ fontFamilies ["Consolas","Monaco","Lucida Console","Liberation Mono"
                       ,"DejaVu Sans Mono","Bitstream Vera Sans Mono"
                       ,"Courier New"," monospace"]
        , width (pct 95)
        ]
    ]


cssString = compile [css]

editorStyle = Html.CssHelpers.style (.css cssString)