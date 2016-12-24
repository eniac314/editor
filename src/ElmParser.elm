module ElmParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import BetterParser exposing (..)
import Tokenizer exposing (tokenizer, Token, tokError)
import TagAttr exposing (TagName(..), Attr(..), attrnames, tagnames, toAttr, toTag)
import HtmlZipper exposing (HTML, Tag, Tree(..), Path)
import Dict exposing (get)
--import Data.Integer exposing (add, fromInt, Integer)


interpret : String -> Int -> Result String ( HTML, Int )
interpret input n =
    case tokenizer input of
        Err s ->
            Err ("Tokenizer error: " ++ s)

        Ok ts ->
            case parse (parseTag [] n) ts of
                Err s ->
                    Err ("Parser error: " ++ s)

                Ok ( res, rest ) ->
                    if rest == [] then
                        Ok res
                    else
                        Err ("Parser error: unprocessed input")


zero = 0
    --fromInt 0


consumerLS : List Token -> Result String ( Token, List Token )
consumerLS xs =
    case xs of
        [] ->
            Err "no more tokens"

        x :: xs_ ->
            Ok ( x, xs_ )


token : String -> Parser (List Token) Token
token s =
    sat consumerLS (\t -> (.val t) == s)


coma : Parser (List Token) a -> Parser (List Token) a
coma p =
    p +++ (token "," >>* p)


parseTagName : Parser (List Token) TagName
parseTagName =
    item consumerLS
        >>= (\v ->
                case get (.val v) tagnames of
                    Nothing ->
                        failure ("invalid tag name: " ++ (tokError v))

                    Just tn ->
                        return tn
            )


parseAttrName : Parser (List Token) (String -> Attr)
parseAttrName =
    item consumerLS
        >>= (\v ->
                case get (.val v) attrnames of
                    Nothing ->
                        failure ("invalid attribute name: " ++ (tokError v))

                    Just an ->
                        return an
            )


parseStringLiteral : Parser (List Token) String
parseStringLiteral =
    sat consumerLS
        (\t ->
            startsWith "\"" (.val t)
                && endsWith "\"" (.val t)
        )
        >>= (\res -> return (trimQuot (.val res)))


trimQuot s =
    (dropRight 1 (dropLeft 1 s))


parseText : Path -> Int -> Parser (List Token) ( HTML, Int )
parseText path n =
    sat consumerLS (\t -> (.val t) == "text")
        >>* parseStringLiteral
        >>= (\s -> return ( (Node (Tag (Text s) (( TextNode, n ) :: path) []) []), n ))


parseMarkdown : Path -> Int -> Parser (List Token) ( HTML, Int )
parseMarkdown path n =
    sat consumerLS (\t -> (.val t) == "markdown")
        >>* parseStringLiteral
        >>= (\s -> return ( (Node (Tag (Markdown s) (( MarkdownNode, n ) :: path) []) []), n ))


parseStyle : Parser (List Token) Attr
parseStyle =
    let
        parseTuple =
            token "("
                >>* parseStringLiteral
                >>= (\v1 ->
                        token ","
                            >>* parseStringLiteral
                            >>= (\v2 ->
                                    token ")"
                                        >>* return ( v1, v2 )
                                )
                    )
    in
        token "style"
            >>* token "["
            >>* (many (coma parseTuple) [] (::))
            >>= (\res ->
                    token "]"
                        >>* return (Style res)
                )


parseAttr : Parser (List Token) Attr
parseAttr =
    (coma parseAttrName
        >>= (\an ->
                parseStringLiteral
                    >>= (\s -> return (an s))
            )
    )
        +++ (coma parseStyle)


parseAttrList : Parser (List Token) (List Attr)
parseAttrList =
    token "["
        >>* many parseAttr [] (::)
        >>= (\res -> token "]" >>* return res)



--f p = (\path n -> p path (n + 1))


parseTag : Path -> Int -> Parser (List Token) ( HTML, Int )
parseTag path n =
    (coma (parseText path n))
        +++ (coma (parseMarkdown path n))
        +++ ((coma parseTagName)
                >>= (\tn ->
                        parseAttrList
                            >>= (\al ->
                                    (parseTagList (( tn, n ) :: path) (n))
                                        >>= (\( ts, n_ ) -> return ( (Node (Tag tn (( tn, n ) :: path) al) ts), n_ ))
                                )
                    )
            )


parseTagList : Path -> Int -> Parser (List Token) ( List HTML, Int )
parseTagList path n =
    token "["
        >>* many_
                (\( _, n ) ->
                    --parseTag path (add n (fromInt 1))
                    parseTag path (n + 1)
                        >>= \( res, n_ ) -> return ( [ res ], n_ )
                )
                ( [], n )
                (\( t, n ) ( ts, n_ ) -> ( t ++ ts, n_ ))
                []
        >>= (\res -> token "]" >>* return res)
