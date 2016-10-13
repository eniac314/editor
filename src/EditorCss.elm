module EditorCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, span)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Html.Attributes

type CssClasses
    = Pane | Debug | ExplTag | TgName


type CssIds
    = LeftPane | RightPane| Editor | ExplWindow | Prompt
      



css =
    (stylesheet << namespace "editor")
    [ (.) Pane
        [ backgroundColor (rgb 155 155 155)
        , display inlineBlock
        ]
    , (.) Debug
        [ display none]
    , (.) ExplTag 
        [ display inlineBlock
        , margin (em 0)
        , padding (em 0.2)
        , borderStyle solid
        , borderColor (rgb 0 0 0)
        , cursor pointer
        , minWidth (px 50)
        ]
    , (.) ExplTag  
        [ hover
           [ important (backgroundColor (rgb 255 255 255))
           ]
        ]
    
    , (#) LeftPane
        [ width (pct 70)
        ]          
    
    , (#) RightPane
        [ width (pct 30)
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