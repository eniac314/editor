module ElmParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Parser exposing (..)
import List exposing (member, foldr, reverse)
import String exposing (cons)

a = "this ise atest   string"

b = parse (token "this") a

c = parse whitespace "     aaaa"

whitespace = 
    map (foldr (++) "") (some (token " " `or` token "\n"))

wh p = 
  (map (foldr (++) "") (many (token " " `or` token "\n")))
  *> p

test = wh (token "this")
      *> wh (token "is")
      *> wh (token "a")
      *> wh (token "test")
      *> wh (token "string")

test2 = wh (token "this")
      `andThen` (\v1 -> wh (token "is")
      `andThen` (\v2 -> wh (token "a")
      `andThen` (\v3 -> wh (token "test")
      `andThen` (\v4 ->  wh (token "string")
      `andThen` (\v5 -> succeed (foldr (++) "" (v1 :: v2 :: v3 :: v4 :: [v5])))))))

plist : List (Parser r1) -> (r1 -> r2 -> r2) -> r2 -> Parser r2
plist ps f acc = 
  let helper ps rs = 
        case ps of 
          [] -> succeed (foldr f acc (reverse rs))
          (p::ps) -> p `andThen` (\r -> helper ps (r::rs))
  in helper ps []  

res = parse test a
res2 = parse ((token "this" `or` whitespace)
              *> (token "is" `or` whitespace)) a
res3 = parse (plist [(wh (token "this"))
                    ,(wh (token "is"))
                    ,(wh (token "a"))
                    ,(wh (token "test"))
                    ,(wh (token "string"))] (++) "") a
