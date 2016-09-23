module TagAttr exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (fromList, get)


-------------------------------------------------------------------------------
type TagName = 
  P | Input | Img | H1 | H2 | H3 | H4 | H5 | H6 |
  Text String  | Div | A | TextNode

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
    TextNode -> span []

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