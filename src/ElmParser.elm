module ElmParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import BetterParser exposing (..)
import Tokenizer exposing (tokenizer, Token, tokError)
import TagAttr exposing (TagName (..),Attr (..), attrnames, tagnames, toAttr, toTag)
import HtmlZipper exposing (HTML, Tag, Tree (..))
import Dict exposing (get)



-- Elm String to HTML

renderer : Result String HTML -> Html msg
renderer res = 
  let renderer' (Node  tag xs) = 
        toTag (.tagname tag)
              (List.map toAttr (.attr tag))
              (List.map renderer' xs)
  in 
  case res of 
    Err s -> div [] [text "Oh no!", br [] [], text s]
    Ok t  -> renderer' t 



interpret : String -> Result String HTML
interpret input = 
  case tokenizer input of 
    Err s -> Err ("Tokenizer error: " ++ s)
    Ok ts -> case parse (parseTag []) ts of 
               Err s  -> Err ("Parser error: " ++ s) 
               Ok (res,_) -> Ok res

consumerLS xs =
  case xs of
    [] -> Err "no more tokens"
    (x :: xs') -> Ok (x,xs')

token : String -> Parser (List Token) Token
token s = sat consumerLS (\t -> (.val t) == s)

coma : Parser (List Token) a -> Parser (List Token) a 
coma p = p +++ (token "," >>* p)

parseTagName : Parser (List Token) TagName
parseTagName = 
  item consumerLS 
  >>= (\v ->
         case get (.val v) tagnames of 
          Nothing -> 
            failure ("invalid tag name: " ++ (tokError v))
          Just tn -> return tn)

parseAttrName : Parser (List Token) (String -> Attr)
parseAttrName = 
  item consumerLS 
  >>= (\v ->
         case get (.val v) attrnames of 
          Nothing -> 
            failure ("invalid attribute name: " ++ (tokError v))
          Just an -> return an)


parseStringLiteral : Parser (List Token) String
parseStringLiteral = 
   sat consumerLS 
         (\t -> startsWith "\"" (.val t)
                && endsWith "\""(.val t))
   >>= (\res -> return (trimQuot (.val res)))

trimQuot s = (dropRight 1 (dropLeft 1 s))

parseText : List TagName -> Parser (List Token) HTML
parseText path = 
  sat consumerLS (\t -> (.val t) == "text")
  >>* parseStringLiteral
  >>= (\s -> return (Node (Tag (Text s) (TextNode :: path) []) []))

parseStyle : Parser (List Token) Attr
parseStyle = 
  let parseTuple = 
        token "("
        >>* parseStringLiteral
        >>= (\v1 -> token ","
        >>* parseStringLiteral
        >>= (\v2 -> token ")"
        >>* return (v1,v2)))

  in token "style"
     >>* token "["
     >>* (many (coma parseTuple) [] (::))
     >>= (\res -> token "]"
     >>* return (Style res))

parseAttr : Parser (List Token) Attr
parseAttr = (coma parseAttrName
            >>= (\an -> parseStringLiteral
            >>= (\s  -> return (an s)))) +++ (coma parseStyle)

parseAttrList : Parser (List Token) (List Attr)
parseAttrList = token "["
                >>* many parseAttr [] (::)
                >>= (\res -> token "]" >>* return res)

parseTag : List TagName -> Parser (List Token) HTML
parseTag path = 
  (coma (parseText path))

  +++
  
  (
  (coma parseTagName)
  >>= (\tn -> parseAttrList
  >>= (\al -> (parseTagList (tn :: path))
  >>= (\ts  -> return (Node (Tag tn (tn :: path) al) ts))))
  )

parseTagList : List TagName -> Parser (List Token) (List HTML)
parseTagList path = 
  token "["
  >>* many (parseTag path) [] (::)
  >>= (\res -> token "]" >>* return res)
