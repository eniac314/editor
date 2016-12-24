module EditorView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import EditorCss exposing (..)
import TagAttr exposing (..)
--import Data.Integer exposing (Integer)
import HtmlZipper
    exposing
        ( HTML
        , HtmlZipper
        , Path
        , Tag
        , Tree(..)
        , ZipTree(..)
        )
import Types exposing (..)
import String exposing (words)
import TagAttr exposing (TagName)
import Markdown exposing (..)
import CssParser exposing (IndexedCss, toCssString)
import Dict exposing (..)


{ id, class, classList } =
    Html.CssHelpers.withNamespace "editor"


renderEditor : Model -> List (Html Msg)
renderEditor model =
    [ renderPath (.currPath model)
    , Html.form
        []
        [ div
            [ class [ Pane ]
            , id LeftPane
            ]
            [ textarea
                [ onInput Store
                , rows 15
                , cols 45
                , id Prompt
                , spellcheck False
                ]
                [ case (.procString model) of
                    Nothing ->
                        text (.rawString model)

                    Just s ->
                        text s
                ]
            , br [] []
            , button [ onClick Parse, type_ "reset" ] [ text "Parse" ]
            , button [ onClick Debug, type_ "button" ] [ text "Debug" ]
            ]
        , div
            [ id RightPane
            , class [ Pane ]
            ]
            [ explorer model
            ]
        ]
    , renderConsole model
    , Html.form
        []
        [ div
            [ class [ Pane ]
            , id LeftPaneCss
            ]
            [ textarea
                [ onInput StoreCss
                , rows 15
                , cols 45
                , id PromptCss
                , spellcheck False
                ]
                [ case (.procCssString model) of
                    Nothing ->
                        text (.rawCssString model)

                    Just s ->
                        text s
                ]
            , br [] []
            , button [ onClick ParseCss, type_ "reset" ]
                [ text "Parse Css" ]
            ]
        , div
            [ id RightPaneCss
            , class [ Pane ]
            ]
            [ cssExplorer model
            ]
        ]
    , renderCssConsole model
    , div [ classList [ ( "DebugClass", .debug model ) ] ]
        [ text <| toString (.parsedCssData model)
        , text <| toString (.procCssString model)
        ]
    ]


explorer : Model -> Html Msg
explorer model =
    let
        page =
            .page model

        dbug =
            .debug model

        sizeExplorer =
            case .winSize model of
                Nothing ->
                    ( 560, 300 )

                Just { width, height } ->
                    ( width // 2, 300 )

        explWindow tags =
            div [ id ExplWindow ]
                [ tags ]

        tags =
            case page of
                Nothing ->
                    span [] []

                Just zp ->
                    render zp

        render (ZipTree ( t, ctx )) =
            let
                spacer indent =
                    if indent == 0 then
                        ""
                    else
                        " " ++ (spacer (indent - 1))

                colors =
                    List.reverse
                        [ "ivory"
                        , "khaki"
                        , "lavender"
                        , "lavenderblush"
                        , "lightcoral"
                        , "lightgreen"
                        , "lemonchiffon"
                        , "thistle"
                        , "mediumspringgreen"
                        , "lightskyblue"
                        ]

                colorPicker xs =
                    case xs of
                        [] ->
                            (colorPicker colors)

                        x :: xs ->
                            ( x, xs )

                render_ n cs (Node tag xs) =
                    let
                        tn =
                            case (.tagname tag) of
                                TagAttr.Text _ ->
                                    "Text"

                                TagAttr.Markdown _ ->
                                    "Markdown"

                                tn_ ->
                                    toString tn_

                        pth =
                            .path tag

                        ( c, cs_ ) =
                            colorPicker cs
                    in
                        p
                            [ class []
                            , style [ ( "margin", "0.1em" ) ]
                            ]
                            ([ text (spacer n)
                             , button
                                [ class [ ExplTag ]
                                , style [ ( "background-color", c ) ]
                                , onClick (GoTo pth)
                                , type_ "reset"
                                ]
                                [ text tn ]
                             ]
                                ++ [ span [ classList [ ( "DebugClass", dbug ) ] ] [ text (toString pth) ] ]
                                ++ (List.map (render_ (n + 3) cs_) xs)
                            )
            in
                render_ 0 colors t
    in
        div
            [ id Explorer
            , style [ ( "white-space", "pre" ) ]
            ]
            [ explWindow tags
            , button [ onClick Up, type_ "reset" ] [ text "Left" ]
            , button [ onClick Down, type_ "reset" ] [ text "Right" ]
            , button [ onClick Left, type_ "reset" ] [ text "Up" ]
            , button [ onClick Right, type_ "reset" ] [ text "Down" ]
            ]


cssExplorer : Model -> Html Msg
cssExplorer model =
    div
        [ id CssExplorerId
        ]
        [ div [ id CssButtons ]
            [ div [ id CssExplWindow ]
                [ renderCssPos model
                , renderDictContent model
                ]
            , div [ id CssExplButtons ]
                [ button
                    [ classList
                        [ ( "CurrentDict"
                          , (.currentDict <| .cssExplorer model) == CssClass
                          )
                        ]
                    , onClick <| ChangeDict CssClass
                    , type_ "reset"
                    ]
                    [ span [] [ text "Class" ] ]
                , button
                    [ classList
                        [ ( "CurrentDict"
                          , (.currentDict <| .cssExplorer model) == CssIds
                          )
                        ]
                    , onClick <| ChangeDict CssIds
                    , type_ "reset"
                    ]
                    [ span [] [ text "Ids" ] ]
                , button
                    [ classList
                        [ ( "CurrentDict"
                          , (.currentDict <| .cssExplorer model) == CssPseudos
                          )
                        ]
                    , onClick <| ChangeDict CssPseudos
                    , type_ "reset"
                    ]
                    [ span [] [ text "Pseudos" ] ]
                , button
                    [ classList
                        [ ( "CurrentDict"
                          , (.currentDict <| .cssExplorer model) == CssTags
                          )
                        ]
                    , onClick <| ChangeDict CssTags
                    , type_ "reset"
                    ]
                    [ span [] [ text "Tags" ] ]
                , button [ onClick GoToCssTop, type_ "reset" ]
                    [ span [] [ text "Top" ] ]
                ]
            ]
        ]


renderDictContent : Model -> Html Msg
renderDictContent model =
    case .parsedCssData model of
        Err s ->
            span [] []

        Ok pCssData ->
            let
                dict =
                    case (.currentDict <| .cssExplorer model) of
                        CssClass ->
                            .classDict pCssData

                        CssIds ->
                            .idDict pCssData

                        CssPseudos ->
                            .pseudoDict pCssData

                        CssTags ->
                            .tagDict pCssData

                entries =
                    case (.currentPos <| .cssExplorer model) of
                        Top ->
                            Dict.map
                                (\e xs ->
                                    div
                                        [ class [ CssDictEntry ]
                                        , onClick (FilterCss ( e, xs ))
                                        ]
                                        [ text e ]
                                )
                                dict

                        InDict ( ce, _ ) ->
                            Dict.map
                                (\e xs ->
                                    if e == ce then
                                        button
                                            [ class [ CssDictEntry ]
                                            , id CurrentCssDictEntry
                                            , onClick (FilterCss ( e, xs ))
                                            , type_ "reset"
                                            ]
                                            [ text e ]
                                    else
                                        button
                                            [ class [ CssDictEntry ]
                                            , onClick (FilterCss ( e, xs ))
                                            , type_ "reset"
                                            ]
                                            [ text e ]
                                )
                                dict
            in
                div [ id CssDictContent ]
                    (Dict.values entries)


renderCssPos : Model -> Html msg
renderCssPos model =
    let
        pos =
            span
                [ class [ Mono ]
                , id CssPosStr
                ]
                [ case (.currentPos <| .cssExplorer model) of
                    Top ->
                        text "Top of file"

                    InDict ( s, xs ) ->
                        text ("Category " ++ s)
                ]
    in
        div [ id CssPos ]
            [ p [] [ text "Currently viewing: ", pos ]
            ]


renderPath : Path -> Html msg
renderPath path =
    let
        pathStr =
            span
                [ class [ Mono ]
                , id PathStr
                ]
                [ text
                    (String.join "/"
                        (List.reverse
                            (List.map (\( t, p ) -> toString t) path)
                        )
                    )
                ]
    in
        div [ id Path ]
            [ p [] [ text "Current path: ", pathStr ]
            ]


renderConsole : Model -> Html msg
renderConsole model =
    div
        [ id Console
        ]
        [ div [ class [ Mono ] ]
            [ case (.parsedData model) of
                Err s ->
                    span [ class [ Error ] ] [ text s ]

                Ok r ->
                    text "parsing complete"
            ]
        ]


renderCssConsole : Model -> Html msg
renderCssConsole model =
    div
        [ id Console
        ]
        [ div [ class [ Mono ] ]
            [ case (.parsedCssData model) of
                Err s ->
                    span [ class [ Error ] ] [ text s ]

                Ok r ->
                    text "parsing complete"
            ]
        ]



-- Elm String to HTML


renderer : Result String ( HTML, Int ) -> Result String IndexedCss -> Html msg
renderer res cssRes =
    let
        renderer_ (Node tag xs) =
            toTag (.tagname tag)
                (List.map toAttr (.attr tag))
                (List.map renderer_ xs)

        css =
            case cssRes of
                Err _ ->
                    ""

                Ok indCss ->
                    toCssString indCss
    in
        case res of
            Err s ->
                div [] [ text "Oh no!", br [] [], text s ]

            Ok ( t, n ) ->
                renderer_ (addStyle css t)


addStyle : String -> HTML -> HTML
addStyle css (Node tag xs) =
    Node tag (xs ++ [ (Node (Tag (CssTag css) [] []) []) ])
