module Tokenizer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, foldl, reverse, head)
import String exposing (cons
                       , words
                       , lines
                       , endsWith
                       , startsWith
                       , dropLeft
                       , dropRight
                       , uncons
                       , isEmpty
                       , fromChar
                       , toList)

import Dict exposing (fromList, get)

--------------------------------------------------------------------------------
--TOKENIZER

type alias Token = 
  { val  : String
  , ch   : Int
  , ln   : Int
  }

tokError : Token -> String
tokError t = "{ val: " ++ (.val t) 
             ++ ", char " ++ (toString (.ch t))
             ++ ", line " ++ (toString (.ln t))
             ++ " }"

tokenizer : String -> Result String (List Token)
tokenizer s = tagPos s |> tokenizer' 

isSpace : Char-> Bool
isSpace c = 
  c == '\r' ||
  c == ' ' ||
  c == '\t' ||
  c == '\n'

isTokenChar : Char-> Bool
isTokenChar c = 
  c == '(' ||
  c == ')' ||
  c == '{' ||
  c == '}' ||
  c == '+' ||
  c == '-' ||
  c == '/' ||
  c == '*' ||
  c == ';' ||
  c == ',' ||
  c == '.' ||
  c == '[' ||
  c == ']' ||
  c == '&' ||
  c == '|' ||
  c == '>' ||
  c == '<' ||
  c == '=' ||
  c == '~'  



  -----------------------------------------------------------------------------

type alias CharPos = 
  { ch  : Char
  , lnp : Int
  , chp : Int
  }

type alias CharMeta = List CharPos

tagPos : String -> CharMeta
tagPos s = 
  let ls = addIndexes (lines s)
      cs = List.map
            (\(n,l) -> toList l  
                       |> addIndexes
                       |> List.map 
                           (\(m,c) -> CharPos c n m)) ls 
  in List.concat cs 


getStringLit : CharMeta -> Result String (CharMeta,CharMeta)
getStringLit xs = 
  case xs of 
    [] -> Err "getStringLit: Invalid String literal"
    --(c::c'::xs) -> 
    --  if (.ch c == '\\') && (.ch c' == '\"')
    --  then 
    --    case getStringLit xs of 
    --        Err s -> Err s
    --        Ok (l,r) -> Ok (c'::l, r)
    --    else if (.ch c' == '\"')
    --         then  Ok ([],xs)
    --         else 
    --          case getStringLit (c'::xs) of 
    --            Err s -> Err s
    --            Ok (l,r) -> Ok (c::l, r)
    (x::xs) -> 
      if (.ch x == '\"')
      then Ok ([],xs)
      else 
        case getStringLit xs of 
          Err s -> Err s
          Ok (l,r) -> Ok (x::l, r) 


tokenString : Result String (CharMeta, CharMeta) -> Result String (Token,CharMeta)
tokenString xs = 
  case xs of 
    Err s -> Err s
    Ok ([],_) -> Err "tokenString: Invalid String literal" 
    Ok ((c::cs),rest) ->
      let ch = .chp c
          ln = .lnp c
          val = foldr (\c s -> cons (.ch c) s) "" (c::cs)
      in Ok ((Token ("\"" ++ val ++ "\"") ch ln),rest)


getTokens' : CharMeta -> (List Token,CharMeta)
getTokens' xs = 
  let tokenize buff = 
        let ys = List.reverse buff
        in case ys of 
             [] -> []
             (y::ys) -> 
             [Token (tString (y::ys)) (.chp y) (.lnp y)]

      tString xs = String.fromList (List.map (.ch) xs) 

      helper xs acc = 
        case xs of 
        [] -> (tokenize acc,[])
        (x::xs) -> 
         if (isTokenChar (.ch x))
         then 
          (tokenize acc ++
           [Token (fromChar (.ch x)) (.chp x) (.lnp x)]
          , xs
          )
         else if (isSpace (.ch x))
              then (tokenize acc, xs)
              else if (.ch x == '\"')
                   then (tokenize acc, x::xs)
                   else helper xs (x::acc)
  in helper xs [] 
  




tokenizer' : CharMeta -> Result String (List Token)
tokenizer' cs = 
  case cs of 
    [] -> Ok []
    (x::xs) -> 
      if (.ch x == '\"')
      then 
        case tokenString (getStringLit xs) of
          Err s -> Err s 
          Ok (t,rest) -> 
            case tokenizer' rest of
              Err s  -> Err s
              Ok ts' -> Ok (t :: ts')
      
      else 
        if isSpace (.ch x)
        then tokenizer' xs
        
        else 
          let (ts,rest) = getTokens' (x::xs)
          in case tokenizer' rest of
              Err s  -> Err s
              Ok ts' -> Ok (ts ++ ts')   
          
          

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


break : (a -> Bool) -> List a -> (List a, List a)
break p xs = 
  let 
    helper ys left = 
      case ys of 
        [] -> (left,[])
        (y::ys) -> 
          if p y 
          then (List.reverse (y :: left), ys)
          else helper ys (y :: left)
  in helper xs []
