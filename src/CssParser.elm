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


isDelim : String -> Bool
isDelim c = 
  c == "(" ||
  c == ")" ||
  c == "{" ||
  c == "}" ||
  c == "+" ||
  c == "-" ||
  c == "/" ||
  c == "*" ||
  c == ";" ||
  c == ":" ||
  c == "," ||
  c == "." ||
  c == "[" ||
  c == "]" ||
  c == "&" ||
  c == "|" ||
  c == ">" ||
  c == "<" ||
  c == "=" ||
  c == "~" 

consumerLS : List Token -> Result String (Token, List Token)
consumerLS xs =
  case xs of
    [] -> Err "no more tokens"
    (x :: xs') -> Ok (x,xs')

safeItem = sat consumerLS (\v -> not (isDelim <| .val v))

token : String -> Parser (List Token) Token
token s = sat consumerLS (\t -> (.val t) == s)

parsePseudoSelector : Parser (List Token) Selector
parsePseudoSelector = 
  token ":"
  >>* safeItem
  >>= (\res -> return <| Pseudo  (":" ++ .val res))

parseClassSelector : Parser (List Token) Selector
parseClassSelector = 
  token "."
  >>* safeItem
  >>= (\res -> return <| Class  ("." ++ .val res))

parseIdSelector : Parser (List Token) Selector
parseIdSelector = 
  token "#"
  >>* safeItem
  >>= (\res -> return <| Id  ("#" ++ .val res))

parseTagSelector : Parser (List Token) Selector
parseTagSelector =
  safeItem
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
  safeItem 
  >>= \v1 -> many ( token "-" 
                    >>* safeItem 
                    >>= \v -> return ( "-" ++ .val v)
                  ) [] (::)
  >>= \res -> return (.val v1 ++ (String.concat res))  

parseValue : Parser (List Token) Value
parseValue = 
  many1 safeItem [] (::)
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
        (\v (classDict, idDict, pseudoDict, tagDict) -> 
          case v of 
            Class s  -> ( update s (cons' id) classDict
                        , idDict, pseudoDict, tagDict)
            Id s     -> ( classDict, update s (cons' id) idDict
                        , pseudoDict, tagDict)
            Pseudo s -> (classDict, idDict
                        , update s (cons' id) pseudoDict, tagDict)
            Tag s    -> (classDict, idDict, pseudoDict
                        , update s (cons' id) tagDict)
            _        -> (classDict, idDict, pseudoDict, tagDict)
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

toCssString : IndexedCss -> String
toCssString indexedCss = 
  let nodes = .cssDict indexedCss
      
      selectorToString s = 
        case s of 
          Class s -> s ++ " "
          Id s -> s ++ " "
          Pseudo s -> s ++ " "
          Tag s -> s ++ " "
          Coma -> ", "
      
      selectorsToString xs = 
        String.join "" (List.map selectorToString xs)

      declarationToString d =
        String.join "" <| 
          List.map (\(p,v) -> "  " ++ p ++ ": " ++ v ++ ";") d 


      nodeToString _ {selectors, declaration} = 
        selectorsToString selectors ++ "{\n" ++
        declarationToString declaration ++ "\n}"
  
  in Dict.foldl (\k v acc -> acc ++ v ++ "\n\n")
                ""
                (Dict.map nodeToString nodes)


interpretCss : String -> Result String IndexedCss
interpretCss input = 
  case tokenizer input of 
    Err s -> Err ("Tokenizer error: " ++ s)
    Ok ts -> case parse parseCss ts of 
               Err s  -> Err ("Parser error: " ++ s) 
               Ok (res,rest) -> 
                if rest == []
                then Ok res
                else Err ("Parser error: unprocessed input")

parserTester p input =
  case tokenizer input of 
    Err s -> Err ("Tokenizer error: " ++ s)
    Ok ts -> case parse p ts of 
               Err s  -> Err ("Parser error: " ++ s) 
               Ok (res,_) -> Ok res 

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

