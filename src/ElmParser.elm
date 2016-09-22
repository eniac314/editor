module ElmParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import Dict exposing (fromList, get)
import BetterParser exposing (..)
import Tokenizer exposing (tokenizer, Token, tokError)

---------------------------------------------------------------------------

type Tree a = Node a (List (Tree a)) | Empty

add : a -> Tree a -> Tree a
add v t = 
  case t of 
    Empty -> Node v []
    Node k xs -> Node k ((Node v [])::xs)

-------------------------------------------------------------------------------

type alias HTML = Tree Tag
type alias Tag = 
  { tagname : TagName
  , attr : List Attr
  }

-------------------------------------------------------------------------------
type TagName = 
  P | Input | Img | H1 | H2 | H3 | H4 | H5 | H6 |
  Text String  | Div | A 

tagnames = 
  fromList [ ("p",P)
           , ("input",Input)
           , ("img",Img)
           , ("h1",H1)
           , ("h2",H2)
           , ("h3",H3)
           , ("h4",H4)
           , ("h5",H5)
           , ("h6",H6)
           , ("div",Div)
           , ("a",A)
           ]

toTag tn xs =
  case tn of 
    Div -> div xs
    P   -> p  xs
    Input -> input xs
    Img   -> img xs
    H1    -> h1 xs
    H2    -> h2 xs
    H3    -> h3 xs
    H4    -> h4 xs
    H5    -> h5 xs
    H6    -> h6 xs
    A     -> a  xs
    Text s -> (\_ -> text s)

-------------------------------------------------------------------------------

type Attr = 
   Class String
 | Id String
 | Style (List (String, String))
 | Href String

attrnames =
  fromList [("class",Class)
           ,("id",Id)
           ,("href",Href)
           ]

toAttr a = 
  case a of 
    Class s -> class s
    Id s    -> id s
    Href s  -> href s
    Style xs -> style xs


-------------------------------------------------------------------------------
-- Elm String to HTML

renderer : Result String HTML -> Html msg
renderer res = 
  let renderer' t = 
    case t of 
      Empty -> text ""
      Node  tag xs -> 
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
    Ok ts -> case parse parseTag ts of 
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

parseText : Parser (List Token) HTML
parseText = 
  sat consumerLS (\t -> (.val t) == "text")
  >>* parseStringLiteral
  >>= (\s -> return (Node (Tag (Text s) []) []))

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

parseTag : Parser (List Token) HTML
parseTag = 
  (coma parseText)

  +++
  
  (
  (coma parseTagName)
  >>= (\tn -> parseAttrList
  >>= (\al -> parseTagList
  >>= (\ts  -> return (Node (Tag tn al) ts))))
  )

parseTagList : Parser (List Token) (List HTML)
parseTagList = token "["
                >>* many parseTag [] (::)
                >>= (\res -> token "]" >>* return res)
