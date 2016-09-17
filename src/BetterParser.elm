module BetterParser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (member, foldr, reverse, head)
import String exposing (cons, words, endsWith, startsWith)
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
-------------------------------------------------------------------------------
-- Elm String to HTML


consumerLS xs =
  case xs of
    [] -> Err "no more tokens"
    (x :: xs') -> Ok (x,xs')

token : String -> Parser (List String) String
token t = sat consumerLS (\s -> s == t)

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
                >>= return

      multiWords = 
        parseStart
        >>= (\st -> many parseMid  "" f
        >>= (\mid -> parseEnd  
        >>= (\end -> return (st ++ " " ++ mid ++ end))))

  in (oneWord +++ multiWords)

parseText : Parser (List String) HTML
parseText = 
  sat consumerLS (\t -> t == "text")
  >>* parseStringLiteral
  >>= (\s -> return (Node (Tag (Text s) []) []))

parseAttr : Parser (List String) Attr
parseAttr = (parseAttrName +++ (token "," >>* parseAttrName))
            >>= (\an -> parseStringLiteral
            >>= (\s  -> return (an s)))

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
  >>= (\tn -> parseAttrList
  >>= (\al -> parseTagList
  >>= (\ts  -> return (Node (Tag tn al) ts))))
  )

parseTagList : Parser (List String) (List HTML)
parseTagList = token "["
                >>* many parseTag [] (::)
                >>= (\res -> token "]" >>* return res)

res = parse parseTag (words testinput)
res2 = parse parseAttrList (words testinput3)

testinput = 
  """ div [ class "mainDiv" ]
          [ p [ ] [ text "this is a test" 
                  , p [ ] [ ]
                  ]
          , p [ ] [ h2 [ id "very important" ] [ ] ]
          ]

  """
testinput2 = 
   """ div [ class "mainDiv" , id "toto" ]
           [ text "hello!" ]

  """

testinput3 = 
   """ [ class "mainDiv" , id "toto" ]
   """
