module BetterParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse, head)
import String exposing (cons, words, endsWith, startsWith, dropLeft, dropRight, uncons, isEmpty, fromChar)
import Dict exposing (fromList, get)

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

plist : List (Parser a b)-> (b -> c -> c) -> c -> Parser a c
plist ps f acc = 
  let helper ps rs = 
        case ps of 
          [] -> return (foldr f acc (reverse rs))
          (p::ps) -> p >>= (\r -> helper ps (r::rs))
  in helper ps []
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

type TagName = 
  P | Input | Img | H1 | H2 | H3 | Text String  | Div

tagnames = 
  fromList [ ("p",P)
           , ("input",Input)
           , ("img",Img)
           , ("h1",H1)
           , ("h2",H2)
           , ("h3",H3)
           , ("div", Div)
           ]

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

  



toTag tn xs =
  case tn of 
    Div -> div xs
    P   -> p  xs
    Input -> input xs
    Img   -> img xs
    H1    -> h1 xs
    H2    -> h2 xs
    H3    -> h3 xs
    Text s -> (\_ -> text s)


toAttr a = 
  case a of 
    Class s -> class s
    Id s    -> id s
    Href s  -> href s
    Style xs -> style xs


-------------------------------------------------------------------------------
-- Elm String to HTML

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

token : String -> Parser (List String) String
token t = sat consumerLS (\s -> s == t)

coma : Parser (List String) a -> Parser (List String) a 
coma p = p +++ (token "," >>* p)

parseTagName : Parser (List String) TagName
parseTagName = 
  item consumerLS 
  >>= (\v ->
         case get v tagnames of 
          Nothing -> 
            failure ("invalid tag name: " ++ (toString v))
          Just tn -> return tn)

parseAttrName : Parser (List String) (String -> Attr)
parseAttrName = 
  item consumerLS 
  >>= (\v ->
         case get v attrnames of 
          Nothing -> 
            failure ("invalid attribute name: " ++ (toString v))
          Just an -> return an)

parseStringLiteral : Parser (List String) String
parseStringLiteral = 
  let parseStart = sat consumerLS (\t -> startsWith "\"" t)
      parseMid   = sat consumerLS (\t -> not (endsWith "\"" t))
      parseEnd   = sat consumerLS (\t -> endsWith "\"" t)
      f s1 s2 = s1 ++ " " ++ s2  
  
      oneWord = sat consumerLS 
                     (\t -> startsWith "\"" t
                            && endsWith "\"" t)
                >>= (\res -> return (trimQuot res))

      multiWords = 
        parseStart
        >>= (\st -> many parseMid  "" f
        >>= (\mid -> parseEnd  
        >>= (\end -> return (trimQuot (st ++ " " ++ mid ++ end)))))

  in (oneWord +++ multiWords)

trimQuot s = (dropRight 1 (dropLeft 1 s))

parseText : Parser (List String) HTML
parseText = 
  sat consumerLS (\t -> t == "text")
  >>* parseStringLiteral
  >>= (\s -> return (Node (Tag (Text s) []) []))

parseStyle : Parser (List String) Attr
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

parseAttr : Parser (List String) Attr
parseAttr = (coma parseAttrName
            >>= (\an -> parseStringLiteral
            >>= (\s  -> return (an s)))) +++ (coma parseStyle)

parseAttrList : Parser (List String) (List Attr)
parseAttrList = token "["
                >>* many parseAttr [] (::)
                >>= (\res -> token "]" >>* return res)

parseTag : Parser (List String) HTML
parseTag = 
  (parseText +++ (token "," >>* parseText))

  +++
  
  (
  (parseTagName +++ (token "," >>* parseTagName))
  --coma (parseTagName +++ parseText)
  >>= (\tn -> parseAttrList
  >>= (\al -> parseTagList
  >>= (\ts  -> return (Node (Tag tn al) ts))))
  )

parseTagList : Parser (List String) (List HTML)
parseTagList = token "["
                >>* many parseTag [] (::)
                >>= (\res -> token "]" >>* return res)

res = (tokenizer testinput4)
res2 = parse parseAttrList (words testinput3)
res3 = parse  parseStyle (words testinput4)

testinput = 
  """ div [ class "mainDiv" ]
          [ p [ style [ ( "color" , "red" ) ] ] [ text "this is a test" 
                  , p [ ] [ ]
                  ]
          , p [ ] [ h2 [ id "very important" , style [ ( "color" , "blue" ) ] ] [ text "big title" ] ]
          ]

  """
testinput2 = 
   """ div [ class "mainDiv" , id "toto" ]
           [ text "hello!" ]

  """

testinput3 = 
   """ [ class "mainDiv" , id "toto" ]
   """

testinput4 = 
  """ div []
      [ h2 [] [text "first title"]
      ] 
  
  """

--------------------------------------------------------------------------------
--TOKENIZER

type alias Token = 
  { val  : String
  , ln   : (Int, String)
  }

tokError : Token -> String
tokError t = "{ val: " ++ (.val t) 
             ++ ", line " ++ (toString (fst (.ln t)))
             ++ ", " ++ (toString (snd (.ln t))) ++ " }"

tokenizer : String -> Result String (List String)
tokenizer s = (words s) |> splitter 

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


splitter : List String -> Result String (List String)
splitter xs = 
  case xs of 
    []      -> Ok []
    (y::ys) ->
      if startsWith "\"" y
      then case getStringLiteral xs of 
            Err s       -> Err s
            Ok (sl,out) -> 
              let rest = splitter out
              in  case rest of 
                Err s   -> Err s
                Ok  r   -> Ok (sl::r)
      else case getTokens y of 
            Err s       -> Err s
            Ok ts       -> 
              let rest = splitter ys
              in  case rest of 
                Err s   -> Err s
                Ok  r   -> Ok (ts ++ r)

getStringLiteral : List String -> Result String (String, List String) 
getStringLiteral xs = 
  case xs of 
    
    [] -> Err "Invalid string literal"

    (s :: []) -> 
      case (splitOn (\c -> c == '\"') s) of 
        (a::b::xs) -> Ok (a ++ b, xs)
        --(a::xs) -> Ok (a, xs)
        _ -> Err "Invalid string literal" 
    
    (s::ss) -> 
      case (splitOn (\c -> c == '\"') s) of 
        (a::b::xs) -> Ok (a ++ b, xs ++ ss)
        _ -> case getStringLiteral ss of
               Err s -> Err s
               Ok (end,rest) -> Ok (s ++ " " ++ end, rest)
      --if endsWith "\"" s
      --then Ok (s,[])
      --else Err "Invalid string literal"
    
    --(s::ss) -> 
    --  if endsWith "\"" s
    --  then Ok (s,ss)
    --  else case getStringLiteral ss of
    --         Err s -> Err s
    --         Ok (end,rest) -> Ok (s ++ " " ++ end, rest)

getTokens : String -> Result String (List String)
getTokens xs =
  Ok (splitOn isTokenChar xs)

splitOn :  (Char -> Bool) -> String -> List String
splitOn p s = 
  let 
  helper (inp,out,buff) = 
      case uncons inp of
        Nothing -> (inp, addBuffer buff out, buff)
        Just (c,ss) ->
          if p c 
          then helper (ss, (fromChar c) :: (addBuffer buff out), "")
          else helper (ss,out,cons c buff)
  
  addBuffer buff out = 
    if isEmpty buff 
    then out
    else (String.reverse buff) :: out 

  (_,res,_) = helper (s,[],"")

  in List.reverse res 

  