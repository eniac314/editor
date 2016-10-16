module CssParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import BetterParser exposing (..)
import Tokenizer exposing (tokenizer, Token, tokError)
import Dict exposing (..)

type alias CssData = Dict Int CssNode
type alias Property = String
type alias Value = String

type alias Declaration = List (Property, Value)

type Selector = Class String 
              | Id String 
              | Pseudo String
              | Tag String
              | Coma

type alias CssNode = 
   { selectors : List Selector
   , declaration : Declaration
   }

type alias IndexedCss = 
  { cssDict : Dict Int CssNode
  , classDict : Dict  String (List Int)
  , idDict : Dict String (List Int)
  , pseudoDict : Dict String (List Int)
  , tagDict : Dict String (List Int)
  }


consumerLS : List Token -> Result String (Token, List Token)
consumerLS xs =
  case xs of
    [] -> Err "no more tokens"
    (x :: xs') -> Ok (x,xs')

token : String -> Parser (List Token) Token
token s = sat consumerLS (\t -> (.val t) == s)

parsePseudoSelector : Parser (List Token) Selector
parsePseudoSelector = 
  token ":"
  >>* (item consumerLS)
  >>= (\res -> return <| Pseudo  (":" ++ .val res))

parseClassSelector : Parser (List Token) Selector
parseClassSelector = 
  token "."
  >>* (item consumerLS)
  >>= (\res -> return <| Class  ("." ++ .val res))

parseIdSelector : Parser (List Token) Selector
parseIdSelector = 
  token "#"
  >>* (item consumerLS)
  >>= (\res -> return <| Id  ("#" ++ .val res))

parseTagSelector : Parser (List Token) Selector
parseTagSelector =
  item consumerLS
  >>= (\res -> return <| Tag  (.val res)) 

parseComa : Parser (List Token) Selector
parseComa = 
  token "," >>* return Coma

parseSelector : Parser (List Token) Selector
parseSelector = 
  parseClassSelector +++
  parseIdSelector +++
  parsePseudoSelector +++
  parseTagSelector


parseSelectors : Parser (List Token) (List Selector)
parseSelectors =
  parseSelector
  >>= \s1 -> many
              (
              (parseComa +++ parseSelector)
               +++ 
              parseSelector
              ) [] (::)
  >>= \ss -> token "{"
  >>* return (s1 :: ss)


parseProperty : Parser (List Token) Property
parseProperty = 
  item consumerLS 
  >>= \v1 -> many ( token "-" 
                    >>* item consumerLS 
                    >>= \v -> return ( "-" ++ .val v)
                  ) [] (::)
  >>= \res -> return (.val v1 ++ (String.concat res))  

parseValue : Parser (List Token) Value
parseValue = 
  many1 (item consumerLS) [] (::)
  >>= \res -> token ";"
  >>* return (String.join " " <| List.map .val res)

parseDeclaration : Parser (List Token) Declaration
parseDeclaration = 
  many1 ( parseProperty 
        >>= \p -> token ":"
        >>* parseValue
        >>= \v -> return (p,v))
        [] (::)
  >>= return 

parseCssNode : Parser (List Token) CssNode
parseCssNode = 
  parseSelectors
  >>= \v1 -> parseDeclaration
  >>= \v2 -> token "}"
  >>* return (CssNode v1 v2)

parseCss : Parser (List Token) IndexedCss
parseCss = 
  many1 parseCssNode [] (::)
  >>= \res -> return (toIndexedCss res)


toIndexedCss : List CssNode -> IndexedCss
toIndexedCss xs = 
  let indexedNodes =  addIndexes xs
      
      cons' x mv =
        case mv of 
          Nothing -> Just [x]
          Just xs -> Just (x::xs) 

      populate (id,node) (classDict, idDict, pseudoDict, tagDict) = 
        List.foldl 
        (\v acc -> 
          case v of 
            Class s  -> ( update s (cons' id) classDict
                        , idDict, pseudoDict, tagDict)
            Id s     -> ( classDict, update s (cons' id) idDict
                        , pseudoDict, tagDict)
            Pseudo s -> (classDict, idDict
                        , update s (cons' id) pseudoDict, tagDict)
            Tag s    -> (classDict, idDict, pseudoDict
                        , update s (cons' id) tagDict)
            _        -> acc
        ) (classDict, idDict, pseudoDict, tagDict) (.selectors node)

      (classDict, idDict, pseudoDict, tagDict) = 
        List.foldl populate 
                   (Dict.empty, Dict.empty, Dict.empty, Dict.empty)
                   indexedNodes
  
  in IndexedCss (Dict.fromList indexedNodes)
                classDict
                idDict
                pseudoDict
                tagDict                      



-------------------------------------------------------------------------------
addIndexes : List a -> List (Int , a)
addIndexes xs = 
  let helper n xs = 
    case xs of
     [] -> []
     (x::xs) -> (n,x) :: helper (n+1) xs
  in helper 0 xs

removeIndexes : List (Int, a) -> List a
removeIndexes xs = List.map snd xs  

