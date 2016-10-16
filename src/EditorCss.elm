module EditorCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, span,p,a, div)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Html.Attributes

type CssClasses
    = Pane | DebugClass | ExplTag | TgName |
      Mono | Error | CurrentPos


type CssIds
    = Explorer | LeftPane | RightPane |
      LeftPaneCss |
      Container | ExplWindow | Prompt |
      Path | PathStr | Menu | PromptCss |
      Console | RendererId
      



css =
    (stylesheet << namespace "editor")
    [ (.) Pane
        [ backgroundColor (rgb 155 155 155)
        , display inlineBlock
        ]
    , (.) DebugClass
        [ display none]
    , (.) ExplTag 
        [ display inlineBlock
        , margin (em 0)
        , padding (em 0.2)
        , borderStyle solid
        , borderColor (rgb 0 0 0)
        , borderTopWidth (em 0.1)
        , borderLeftWidth (em 0.1)
        , cursor pointer
        , minWidth (px 50)
        ]
    , (.) ExplTag  
        [ hover
           [ important (backgroundColor (rgb 255 255 255))
           ]
        ]
    , (.) Mono
        [ fontFamilies mono ]
    , (.) Error
        [ color (rgb 255 0 0)]
    , (.) CurrentPos
        [ important <| color (rgb 255 255 255)
        , important <| backgroundColor (rgb 70 130 180) 
        ]

    , (#) Container
        [ backgroundColor (rgb 70 130 180)
        ]
    , (#) Menu
        [ backgroundColor (rgb 155 155 155)
        , padding (em 0.2)
        , children
           [ a 
             [ display inlineBlock
             , textDecoration none
             , backgroundColor (rgb 255 255 255) 
             , margin (em 0.2)
             , paddingLeft (em 0.2)
             , paddingRight (em 0.2)
             , color (rgb 70 130 180)
             , hover 
                [ color (rgb 234 21 125)
                , cursor pointer
                ]
             ]
           ]
        ]
    , (#) Path
        [ backgroundColor (rgb 155 155 155)
        , padding (em 0.2)
        , children
           [ p 
             [ backgroundColor (rgb 255 255 255) 
             , margin (em 0.2)
             ]
           ]
        ]
    , (#) PathStr
        [ color (rgb 70 130 180)]
    , (#) LeftPane
        [ width (pct 70)
        ]          
    , (#) LeftPaneCss
        [ width (pct 70)
        ]
    , (#) RightPane
        [ width (pct 30)
        ]
 
    , (#) ExplWindow
        [ width (pct 100)
        , height (px 287)
        , overflow scroll
        ]
    , (#) Explorer
        [ width (pct 95)
        ]
    , (#) Prompt 
        [ fontFamilies mono
        , width (pct 95)
        ]
    , (#) PromptCss 
        [ fontFamilies mono
        , width (pct 95)
        ]
    , (#) Console
        [ backgroundColor (rgb 155 155 155)
        , padding (em 0.2)
        , children
           [ div 
             [ backgroundColor (rgb 255 255 255) 
             , margin (em 0.2)
             ]
           ]
        ]
    , rendererCss
    ]

rendererCss = 
    (#) RendererId
      [ backgroundColor (rgb 255 255 255)
      ]

mono = ["Consolas","Monaco","Lucida Console","Liberation Mono"
       ,"DejaVu Sans Mono","Bitstream Vera Sans Mono"
       ,"Courier New"," monospace"]

cssString = compile [css]

editorStyle = Html.CssHelpers.style (.css cssString)