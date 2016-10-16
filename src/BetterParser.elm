module BetterParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse, head)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import Dict exposing (fromList, get)
import Tokenizer exposing (..)

type alias Res b a = Result String (b,a)
type Parser a b = Parser (a -> Res b a)
type alias Consumer a b = (a -> Res b a)

parse : Parser a b -> (a -> Res b a)
parse (Parser p) = p

return : b -> Parser a b
return v = Parser (\ts -> Ok (v,ts)) 

(>>=) : Parser a b -> (b -> Parser a c) -> Parser a c
(>>=) p f = 
    Parser (\ts ->
             case (parse p) ts of
                Err s -> Err s
                --Err s -> Err (s ++ " " ++ toString ts)
                Ok (v,out) -> parse (f v) out            
            )

(>>*) : Parser a b -> Parser a c -> Parser a c
(>>*) p q = p >>= (\_ -> q) 

item : Consumer a b -> Parser a b
item consume  = Parser consume

failure : String -> Parser a b
failure s = Parser (\_ -> Err ("Failure: " ++ s))

sat : Consumer a b -> (b -> Bool) -> Parser a b
sat f p = (item f) >>=
           \x -> if p x
                 then return x
                 else failure (toString x)

(+++) : Parser a b -> Parser a b -> Parser a b
(+++) p q = Parser (\ts -> 
                     case parse p ts of
                        Err s -> parse q ts
                        Ok res -> Ok res)

many :  Parser a b -> c -> (b -> c -> c) -> Parser a c
many p acc f = (many1 p acc f) +++ return acc

many1 :  Parser a b -> c -> (b -> c -> c) -> Parser a c
many1 p acc f = 
  p >>= (\v -> many p acc f >>= (\vs -> return (f v vs)))

many' : (( a, b ) -> Parser c ( a, b )) -> ( a, b ) -> (( a, b ) -> ( d, b ) -> ( d, b )) -> d -> Parser c ( d, b )
many' p acc f ie = (many1' p acc f ie) +++ return (ie,snd acc)

many1' : (( a, b ) -> Parser c ( a, b )) -> ( a, b ) -> (( a, b ) -> ( d, b ) -> ( d, b )) -> d -> Parser c ( d, b )
many1' p acc f ie = 
  p acc >>= (\v -> many' p v f ie >>= (\vs -> return (f v vs)))

plist : List (Parser a b)-> (b -> c -> c) -> c -> Parser a c
plist ps f acc = 
  let helper ps rs = 
        case ps of 
          [] -> return (foldr f acc (reverse rs))
          (p::ps) -> p >>= (\r -> helper ps (r::rs))
  in helper ps []


