module HtmlZipper exposing (..)

import TagAttr exposing (TagName, Attr, splitAttr)
import Dict exposing (get)
import String exposing (toLower, dropRight, dropLeft, lines, join)
--import Data.Integer exposing (add, fromInt, Integer)


---------------------------------------------------------------------------


type Tree a
    = Node a (List (Tree a))


type Context a
    = Context a (List (Tree a)) (List (Tree a))


type ZipTree a
    = ZipTree ( Tree a, List (Context a) )


initZip : Tree a -> ZipTree a
initZip t =
    ZipTree ( t, [] )


extractTree : ZipTree a -> Tree a
extractTree (ZipTree ( Node fVal ts, ctxs )) =
    Node fVal ts


extractTag : ZipTree Tag -> Tag
extractTag (ZipTree ( Node fVal ts, ctxs )) =
    fVal


extractPath : ZipTree Tag -> Path
extractPath z =
    .path (extractTag z)


root : Path -> Maybe ( TagName, Int )
root path =
    List.head path


updateTag : HTML -> HtmlZipper -> HtmlZipper
updateTag (Node tag1 ts) (ZipTree ( Node tag2 _, ctx )) =
    let
        path =
            case (.path tag2) of
                [] ->
                    .path tag1

                x :: xs ->
                    (.path tag1) ++ xs
    in
        ZipTree ( Node (Tag (.tagname tag1) path (.attr tag1)) ts, ctx )


updateFocus : Tree a -> ZipTree a -> ZipTree a
updateFocus t (ZipTree ( oldT, ctx )) =
    (ZipTree ( t, ctx ))


zipUp : ZipTree a -> Maybe (ZipTree a)
zipUp (ZipTree ( fTree, ctxs )) =
    case ctxs of
        [] ->
            Nothing

        (Context parent ls rs) :: bs ->
            Just (ZipTree ( Node parent (ls ++ [ fTree ] ++ rs), bs ))


zipDown : (Tree a -> Bool) -> ZipTree a -> Maybe (ZipTree a)
zipDown p (ZipTree ( Node fVal ts, ctxs )) =
    case ts of
        [] ->
            Nothing

        ts ->
            let
                ( l, r ) =
                    break p ts
            in
                case r of
                    [] ->
                        Nothing

                    t :: ts_ ->
                        Just (ZipTree ( t, (Context fVal l ts_) :: ctxs ))


zipDownFirst : ZipTree a -> Maybe (ZipTree a)
zipDownFirst (ZipTree ( Node fVal ts, ctxs )) =
    case ts of
        [] ->
            Nothing

        x :: xs ->
            Just (ZipTree ( x, (Context fVal [] xs) :: ctxs ))


zipRight : ZipTree a -> Maybe (ZipTree a)
zipRight (ZipTree ( fTree, ctxs )) =
    case ctxs of
        [] ->
            Nothing

        (Context parent ls rs) :: bs ->
            case rs of
                [] ->
                    Nothing

                t :: ts ->
                    Just (ZipTree ( t, (Context parent (ls ++ [ fTree ]) ts) :: bs ))


zipLeft : ZipTree a -> Maybe (ZipTree a)
zipLeft (ZipTree ( fTree, ctxs )) =
    case ctxs of
        [] ->
            Nothing

        (Context parent ls rs) :: bs ->
            case (List.reverse ls) of
                [] ->
                    Nothing

                t :: ts ->
                    Just
                        (ZipTree
                            ( t
                            , (Context parent (List.reverse ts) (fTree :: rs)) :: bs
                            )
                        )


cd : List b -> (a -> Maybe b) -> ZipTree a -> Maybe (ZipTree a)
cd path f zt =
    case path of
        [] ->
            Nothing

        p :: [] ->
            let
                g (Node val _) =
                    (case (f val) of
                        Nothing ->
                            False

                        Just p_ ->
                            p_ == p
                    )
            in
                (zipDown g zt)

        p :: ps ->
            let
                g (Node val _) =
                    (case (f val) of
                        Nothing ->
                            False

                        Just p_ ->
                            p_ == p
                    )
            in
                case (zipDown g zt) of
                    Nothing ->
                        Nothing

                    Just zt_ ->
                        cd ps f zt_


cd_ path zt =
    let
        current =
            List.reverse (extractPath zt)

        dest =
            List.reverse path

        directions =
            trimPath current dest

        trimPath cs ds =
            case ( cs, ds ) of
                ( [], [] ) ->
                    []

                ( c :: cs, [] ) ->
                    []

                ( [], ds ) ->
                    ds

                ( c :: cs_, d :: ds_ ) ->
                    if c == d then
                        trimPath cs_ ds_
                    else
                        []

        hasSameName d (Node tag ts) =
            let
                tn =
                    .path tag
            in
                case root tn of
                    Nothing ->
                        False

                    Just r ->
                        r == d

        helper dest zt =
            case dest of
                [] ->
                    Just zt

                d :: ds ->
                    case (zipDown (hasSameName d) zt) of
                        Nothing ->
                            Nothing

                        Just zt_ ->
                            helper ds zt_
    in
        helper directions zt



-------------------------------------------------------------------------------


type alias HTML =
    Tree Tag


type alias Tag =
    { tagname : TagName
    , path : List ( TagName, Int )
    , attr : List Attr
    }


type alias Path =
    List ( TagName, Int )


htmlToString : HTML -> String
htmlToString html =
    let
        spacer indent =
            if indent == 0 then
                ""
            else
                " " ++ (spacer (indent - 1))

        getT tagname =
            toLower (toString tagname)

        getA attrname =
            toLower (toString attrname)

        attrListToString ats =
            List.map
                (\a ->
                    let
                        ( an, payload ) =
                            splitAttr a
                    in
                        (trimQuot (getA an)) ++ " " ++ payload
                )
                ats

        helper indent (Node { tagname, path, attr } childs) =
            let
                tn =
                    getT tagname

                buff =
                    spacer (indent + off)

                off =
                    (String.length tn)

                atList =
                    attrListToString attr

                tglist =
                    (List.map (helper (indent + off + 3)) childs)

                atln =
                    if atList == [] then
                        " []"
                    else
                        " [ "
                            ++ (String.join ("\n" ++ buff ++ " , ") atList)
                            ++ ("\n" ++ buff ++ " ]")

                tgln =
                    if tglist == [] then
                        " []"
                    else
                        " [ "
                            ++ (String.join ("\n" ++ buff ++ " , ") tglist)
                            ++ ("\n" ++ buff ++ " ]")
            in
                case tagname of
                    TagAttr.Text s ->
                        "text \"" ++ s ++ "\""

                    TagAttr.Markdown s ->
                        "markdown \n\"" ++ s ++ "\""

                    _ ->
                        tn
                            ++ atln
                            ++ "\n"
                            ++ buff
                            ++ tgln
    in
        helper 0 html


type alias HtmlZipper =
    ZipTree Tag



--------------------------------------------------------------------------------


trimQuot s =
    (dropRight 1 (dropLeft 1 s))


break : (a -> Bool) -> List a -> ( List a, List a )
break p xs =
    let
        helper ys left =
            case ys of
                [] ->
                    ( left, [] )

                y :: ys ->
                    if p y then
                        ( List.reverse left, y :: ys )
                    else
                        helper ys (y :: left)
    in
        helper xs []
