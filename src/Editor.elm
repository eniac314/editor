module Editor exposing (..)

import Html exposing (..)
import Task exposing (perform)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (words, lines, join)
import Http
import Dict exposing (fromList, toList, get)
import Json.Decode as Json
import Json.Encode exposing (string)
import Task exposing (succeed, perform, attempt)
--import Data.Integer exposing (fromInt, add, Integer)
import Html.CssHelpers
import EditorCss exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, (</>), int, oneOf, s, string)
import Types exposing (..)
import HtmlZipper
    exposing
        ( HTML
        , HtmlZipper
        , Path
        , Tag
        , Tree(..)
        , ZipTree(..)
        , htmlToString
        , initZip
        , updateTag
        , extractTree
        , extractTag
        , extractPath
        , zipUp
        , zipDownFirst
        , zipLeft
        , zipRight
        , cd_
        , root
        )
import TagAttr exposing (TagName)
import ElmParser
    exposing
        ( interpret
        )
import CssParser exposing (interpretCss)
import Svg exposing (svg, rect, text_)
import Svg.Attributes exposing (width, height, viewBox, fill, x, y, class)
import Window as Win
import EditorView exposing (..)
import Pad exposing (..)
import Keyboard exposing (..)
import CssParser exposing (..)


{ id, class, classList } =
    Html.CssHelpers.withNamespace "editor"


main =
    Navigation.program urlParser
        { init = init_
        , update = update
        --, urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }


init_ location =
    let
        ( m, cmd ) = (init testinput5 initCss2, modifyUrl (.hash location))

    in
        ( m
        , Cmd.batch
            [ cmd
            , initWinSize
            , modifyUrl "#editor"
            ]
        )


initWinSize =
    attempt (\res -> case res of
                       Ok s  -> WinSize s
                       Err _ -> Failure)
            Win.size



urlParser : (Location -> Msg)
urlParser =
        (\s ->
            let validUrlList = ["#mainmenu","#editor","#fileIO","#renderer"] 
            in if List.member (.hash s) validUrlList
               then ChangeUrl (.hash s)
               else NewUrl "#editor"    
        )


init initInput initCssInput =
    let
        pdata =
            --interpret initInput (fromInt 0)
            interpret initInput 0

        pCssData =
            interpretCss initCssInput

        initPage =
            case pdata of
                Err s ->
                    Nothing

                Ok ( t, n ) ->
                    Just (initZip t)

        initPath =
            case initPage of
                Nothing ->
                    []

                Just ip ->
                    extractPath ip

        --nextId =
        --    case pdata of
        --        Err s ->
        --            fromInt 0

        --        Ok ( r, n ) ->
        --            (add n (fromInt 1))
        nextId =
            case pdata of
                Err s -> 0

                Ok ( r, n ) ->
                    (n + 1)
    in
        Model Editor
            initInput
            initCssInput
            Nothing
            Nothing
            pdata
            pCssData
            initPath
            (CssExplorer Top CssClass)
            initPage
            (renderer pdata pCssData)
            nextId
            True
            Nothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Store s ->
            { model | rawString = s } ! []

        StoreCss s ->
            { model | rawCssString = s } ! []

        Parse ->
            parse model ! []

        ParseCss ->
            parseCss model ! []

        Up ->
            move zipUp model ! []

        Down ->
            move zipDownFirst model ! []

        Left ->
            move zipLeft model ! []

        Right ->
            move zipRight model ! []

        GoTo path ->
            move (cd_ path) model ! []

        Debug ->
            { model | debug = not (.debug model) } ! []

        Failure ->
            model ! []

        WinSize s ->
            { model | winSize = Just s } ! []

        NewUrl s -> model ! [newUrl s]
        ChangeUrl s ->
            let newPos =
                  if s == "#mainmenu" then MainMenu else
                  if s == "#fileIO" then FileIO else
                  if s == "#renderer" then Renderer else Editor

                  
            in { model | position = newPos } ! []

        ChangeDict d ->
            (changeDict model d) ! []

        SwapEditorRender ->
            model ! [ swapEditorRender model ]

        GoToCssTop ->
            (goToCssTop model) ! []

        FilterCss ( n, xs ) ->
            (filterCss model ( n, xs )) ! []


goToCssTop model =
    case .parsedCssData model of
        Err s ->
            model

        Ok pCssData ->
            let
                dict =
                    .cssDict pCssData

                newProcCssString =
                    Just (toCssString pCssData)

                explorer =
                    (.cssExplorer model)

                newExplorer =
                    { explorer | currentPos = Top }
            in
                { model
                    | cssExplorer = newExplorer
                    , procCssString = newProcCssString
                    , toRender =
                        renderer (.parsedData model)
                            (Ok pCssData)
                }


filterCss model ( node, xs ) =
    case .parsedCssData model of
        Err s ->
            model

        Ok pCssData ->
            let
                dict =
                    .cssDict pCssData

                values =
                    List.map (\id -> Dict.get id dict) xs

                newProcString =
                    nodesToCssString (List.reverse values)

                explorer =
                    (.cssExplorer model)

                newExplorer =
                    { explorer | currentPos = InDict ( node, xs ) }
            in
                { model
                    | procCssString = Just newProcString
                    , cssExplorer = newExplorer
                }


changeDict model d =
    let
        explorer =
            (.cssExplorer model)

        newExplorer =
            { explorer | currentDict = d }
    in
        { model | cssExplorer = newExplorer }


swapEditorRender model =
    case .position model of
        Editor ->    
            newUrl "#renderer"

        Renderer ->
            newUrl "#editor"

        _ ->
            Cmd.none


parse model =
    let
        pdata =
            interpret (.rawString model) (.nextId model)

        prString =
            case pdata of
                Err s ->
                    Nothing

                Ok ( r, n ) ->
                    Just (htmlToString r)

        newPage =
            case pdata of
                Err s ->
                    (.page model)

                Ok ( r, _ ) ->
                    case (.page model) of
                        Nothing ->
                            Just (initZip r)

                        Just p ->
                            Just (updateTag r p)

        nextId =
            case pdata of
                Err s ->
                    .nextId model

                --Ok ( _, n ) ->
                --    add n (fromInt 1)
                Ok ( _, n ) ->
                    ( n + 1)

        currPath =
            case newPage of
                Nothing ->
                    .currPath model

                Just p ->
                    extractPath p
    in
        { model
            | procString = prString
            , parsedData = pdata
            , page = newPage
            , nextId = nextId
            , currPath = currPath
            , toRender = renderer pdata (.parsedCssData model)
        }


parseCss model =
    case (.currentPos <| .cssExplorer model) of
        Top ->
            let
                pCssData =
                    interpretCss (.rawCssString model)

                newProcCssString =
                    case pCssData of
                        Err s ->
                            Nothing

                        Ok indCss ->
                            Just (toCssString indCss)
            in
                { model
                    | parsedCssData = pCssData
                    , procCssString = newProcCssString
                    , toRender =
                        renderer (.parsedData model)
                            pCssData
                }

        InDict ( ce, nodeIndexes ) ->
            let
                resTmpPCssData =
                    interpretCss (.rawCssString model)

                resPCssData =
                    (.parsedCssData model)
            in
                case resPCssData of
                    Err s ->
                        model

                    Ok pCssData ->
                        case resTmpPCssData of
                            Err s ->
                                model

                            Ok tmpPCssData ->
                                let
                                    newProcCssString =
                                        Just (toCssString tmpPCssData)

                                    tmpPCssDataList =
                                        (.cssDict tmpPCssData)
                                            |> Dict.toList
                                            |> (reNumber (List.reverse nodeIndexes) nextInd)

                                    newPCssDataList =
                                        (.cssDict pCssData)
                                            |> (\d -> List.foldl (\k acc -> Dict.remove k acc) d nodeIndexes)
                                            |> Dict.toList
                                            |> (\xs -> insertAt xs tmpPCssDataList)
                                            |> (List.map (\( ind, node ) -> Just node))

                                    newPCssData =
                                        (nodesToCssString newPCssDataList)
                                            |> interpretCss

                                    nextInd =
                                        Maybe.withDefault 0 (List.head nodeIndexes)

                                    reNumber prevPos next xs =
                                        case xs of
                                            [] ->
                                                []

                                            ( id, node ) :: xs ->
                                                case prevPos of
                                                    [] ->
                                                        ( next, node ) :: (reNumber [] (next + 1) xs)

                                                    n :: ns ->
                                                        ( n, node ) :: (reNumber ns next xs)

                                    splitAt n acc xs =
                                        if n == 0 then
                                            ( List.reverse acc, xs )
                                        else
                                            case xs of
                                                [] ->
                                                    ( List.reverse acc, [] )

                                                x :: xs ->
                                                    splitAt (n - 1) (x :: acc) xs

                                    insertAt xs toInsert =
                                        List.foldl
                                            (\( ind, node ) acc ->
                                                let
                                                    ( left, right ) =
                                                        splitAt ind [] acc
                                                in
                                                    (left ++ [ ( ind, node ) ] ++ right)
                                            )
                                            xs
                                            toInsert
                                in
                                    { model
                                        | parsedCssData = newPCssData
                                        , procCssString = newProcCssString
                                        , toRender =
                                            renderer (.parsedData model)
                                                newPCssData
                                    }


move : (HtmlZipper -> Maybe HtmlZipper) -> Model -> Model
move f model =
    let
        newPage =
            case (.page model) of
                Nothing ->
                    Nothing

                Just p ->
                    case (f p) of
                        Nothing ->
                            Just p

                        Just np ->
                            Just np

        newRstring =
            case newPage of
                Nothing ->
                    (.rawString model)

                Just np ->
                    htmlToString (extractTree np)

        newProcString =
            Just newRstring

        newParsedData =
            case newPage of
                Nothing ->
                    Err "wrong Html tree"

                Just np ->
                    Ok ( (extractTree np), .nextId model )

        newPath =
            case newPage of
                Nothing ->
                    (.currPath model)

                Just np ->
                    (extractPath np)

        newRender =
            renderer newParsedData (.parsedCssData model)
    in
        { model
            | rawString = newRstring
            , procString = newProcString
            , parsedData = newParsedData
            , currPath = newPath
            , page = newPage
            , toRender = newRender
        }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id Container
        , setHeight (.winSize model)
        ]
        ([ EditorCss.editorStyle
         , renderMenu model
         ]
            ++ (case (.position model) of
                    MainMenu ->
                        renderMainMenu model

                    Editor ->
                        renderEditor model

                    FileIO ->
                        renderFileIO model

                    Renderer ->
                        renderRenderer model
               )
        )


renderMainMenu model =
    []


renderFileIO model =
    []


renderRenderer model =
    [ div [ id RendererId ]
        [ .toRender model ]
    ]


renderMenu : Model -> Html Msg
renderMenu model =
    div
        [ id Menu
        ]
        [ a
            [ classList [ ( "CurrentPos", (.position model == MainMenu) ) ]
            , onClick (NewUrl "#mainmenu")
            ]
            [ text "Main Menu" ]
        , a
            [ classList [ ( "CurrentPos", (.position model == Editor) ) ]
            , onClick (NewUrl "#editor")
            ]
            [ text "Editor" ]
        , a
            [ classList [ ( "CurrentPos", (.position model == FileIO) ) ]
            , onClick (NewUrl "#fileIO")
            ]
            [ text "Save/Load" ]
        , a
            [ classList [ ( "CurrentPos", (.position model == Renderer) ) ]
            , onClick (NewUrl "#renderer")
            ]
            [ text "Html Preview" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Win.resizes WinSize, presses keyToMsg ]



--Keyboard


keyToMsg : KeyCode -> Msg
keyToMsg k =
    let
        keys =
            Dict.fromList
                [ --(37,Up)
                  --,(39,Down)
                  --,(38,Left)
                  --,(40,Right)
                  --,
                  ( 112, Parse )
                , ( 113, SwapEditorRender )
                ]
    in
        case get k keys of
            Nothing ->
                Failure

            Just msg ->
                msg


setHeight winSize =
    case winSize of
        Nothing ->
            style []

        Just { width, height } ->
            style [ ( "height", toString height ++ "px" ) ]
