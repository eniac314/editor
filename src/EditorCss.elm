module EditorCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Html.Attributes


type CssClasses
    = Pane
    | DebugClass
    | ExplTag
    | TgName
    | Mono
    | Error
    | CurrentPos
    | CssDictEntry
    | CurrentDict


type CssIds
    = Explorer
    | LeftPane
    | RightPane
    | LeftPaneCss
    | RightPaneCss
    | Container
    | ExplWindow
    | Prompt
    | Path
    | PathStr
    | Menu
    | PromptCss
    | Console
    | RendererId
    | CssExplorerId
    | CssButtons
    | CssExplWindow
    | CssDictContent
    | CurrentCssDictEntry
    | CssExplButtons
    | CssPosStr
    | CssPos


css =
    (stylesheet << namespace "editor")
        [ button
            [ cursor pointer
            , hover
                [ color (rgb 70 130 180)
                ]
            ]
        , (.) Pane
            [ backgroundColor (rgb 155 155 155)
            , display inlineBlock
            ]
        , (.) DebugClass
            [ display none ]
        , (.) ExplTag
            [ display inlineBlock
            , margin (em 0)
            , padding (em 0.2)
            , borderStyle solid
            , borderColor (rgb 0 0 0)
            , borderTopWidth (em 0.1)
            , borderLeftWidth (em 0.1)
            , borderRightWidth (em 0.2)
            , borderBottomWidth (em 0.2)
            , cursor pointer
            , minWidth (px 50)
            , color (rgb 0 0 0)
            , hover
                [ important (backgroundColor (rgb 255 255 255))
                ]
            ]
        , (.) Mono
            [ fontFamilies mono ]
        , (.) Error
            [ color (rgb 255 0 0) ]
        , (.) CurrentPos
            [ important <| color (rgb 255 255 255)
            , important <| backgroundColor (rgb 70 130 180)
            ]
        , (.) CurrentDict
            [ important <| color (rgb 70 130 180)
            ]
        , (.) CssDictEntry
            [ display inlineBlock
            , padding (em 0.2)
            , margin (em 0.1)
            , borderStyle solid
            , borderColor (rgb 0 0 0)
            , borderTopWidth (em 0.1)
            , borderLeftWidth (em 0.1)
            , cursor pointer
            , minWidth (px 50)
            , backgroundColor (rgb 70 150 180)
            ]
        , (.) CssDictEntry
            [ hover
                [ important (backgroundColor (rgb 255 255 255))
                ]
            , color (rgb 0 0 0)
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
            [ color (rgb 70 130 180) ]
        , (#) CssPos
            [ backgroundColor (rgb 155 155 155)
            , padding (em 0.2)
            , paddingLeft (em 0)
            , paddingTop (em 0)
            , children
                [ p
                    [ backgroundColor (rgb 255 255 255)
                    , margin (em 0.2)
                    , paddingLeft (em 0.2)
                    ]
                ]
            ]
        , (#) CssPosStr
            [ color (rgb 70 130 180) ]
        , (#) LeftPane
            [ width (pct 65)
            ]
        , (#) LeftPaneCss
            [ width (pct 65)
            ]
        , (#) RightPane
            [ width (pct 35)
            ]
        , (#) RightPaneCss
            [ width (pct 35)
            ]
        , (#) ExplWindow
            [ width (pct 100)
            , height (px 287)
            , overflow scroll
            , overflowX hidden
            , paddingBottom (em 0)
            ]
        , (#) CssExplWindow
            [ width (pct 100)
            , height (px 287)
            , overflow scroll
            , overflowX hidden
            , backgroundColor (rgb 155 155 155)
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
        , (#) CurrentCssDictEntry
            [ backgroundColor (rgb 200 150 150)
            ]
        , (#) CssExplButtons
            [ width (pct 100)
            , children
                [ button
                    [ width (pct 20)
                    , margin (px 0)
                    , padding (px 0)
                    , children
                        [ span
                            [ display inlineBlock
                            , margin (px 0)
                            , padding (px 0)
                            , width (pct 110)
                            ]
                        ]
                      --, fontSize (em 0.8)
                      --, important (padding (em 0.1))
                    ]
                ]
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


mono =
    [ "Consolas"
    , "Monaco"
    , "Lucida Console"
    , "Liberation Mono"
    , "DejaVu Sans Mono"
    , "Bitstream Vera Sans Mono"
    , "Courier New"
    , " monospace"
    ]


cssString =
    compile [ css ]


editorStyle =
    Html.CssHelpers.style (.css cssString)
