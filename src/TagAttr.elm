module TagAttr exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (fromList, toList ,get)
import List exposing (map)
import String exposing (fromList, toList )
import Json.Encode exposing (string)
import Char exposing (toUpper)
import Markdown exposing (..)

-------------------------------------------------------------------------------
type TagName = 
  P | Input | Img | H1 | H2 | H3 | H4 | H5 | H6 |
  Text String  | Div | A | TextNode | MarkdownNode | Hr 
  | Pre | Blockquote | Span | Code | Em | Strong |
  I | B | U | Sub | Sup | Br | Ol | Ul | Li |
  Dl | Dt | Dd | Iframe | Canvas  | Svg | Math |
  Form | Textarea | Button | Select | Option |
  Section | Nav | Article | Aside | Header | 
  Footer | Address | Main | Body | Figure | Figcaption |
  Table | Caption | Colgroup | Col | Tbody | Thead | Tfoot |
  Tr | Td | Th | Markdown String | CssTag String

tagnames = 
  Dict.fromList
           [ ("p",P)
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
           , ("pre",Pre)
           , ("blockquote",Blockquote)
           , ("span",Span)
           , ("code",Code)
           , ("em",Em)
           , ("strong",Strong)
           , ("i",I)
           , ("b",B)
           , ("u",U)
           , ("sub",Sub)
           , ("sup",Sup)
           , ("br",Br)
           , ("ol",Ol)
           , ("ul",Ul)
           , ("li",Li)
           , ("dl",Dl)
           , ("dt",Dt)
           , ("dd",Dd)
           , ("iframe",Iframe)
           , ("canvas",Canvas)
           , ("svg",Svg)
           , ("math",Math)
           , ("form",Form)
           , ("textarea",Textarea)
           , ("button",Button)
           , ("select",Select)
           , ("option",Option )
           , ("section",Section)
           , ("nav",Nav)
           , ("article",Article)
           , ("aside",Aside)
           , ("header",Header)
           , ("footer",Footer)
           , ("address",Address)
           , ("main",Main)
           , ("body",Body)
           , ("Figure",Figure)
           , ("figcaption",Figcaption)
           , ("table",Table)
           , ("caption",Caption)
           , ("colgroup",Colgroup)
           , ("col",Col)
           , ("tbody",Tbody)
           , ("thead",Thead)
           , ("tfoot",Tfoot)
           , ("tr",Tr)
           , ("td",Td)
           , ("th",Th)
           ]

toTag : TagName -> List (Attribute msg) -> List (Html msg) -> Html msg
toTag tn  =
  case tn of 
    Div -> div
    P   -> p  
    Input -> input 
    Img   -> img 
    H1    -> h1 
    H2    -> h2 
    H3    -> h3 
    H4    -> h4 
    H5    -> h5 
    H6    -> h6 
    A     -> a  
    Text s -> (\_ _-> text s)
    TextNode -> (\_ _-> text "should never happen")
    MarkdownNode -> (\_ _-> text "should never happen")
    Hr -> hr
    Pre -> pre
    Blockquote -> blockquote
    Span -> span
    Code -> code
    Em -> em
    Strong -> strong
    I -> i
    B -> b
    U -> u
    Sub -> sub
    Sup -> sup
    Br -> br
    Ol -> ol
    Ul -> ul
    Li -> li
    Dl -> dl
    Dt -> dt
    Dd -> dd
    Iframe -> iframe
    Canvas  -> canvas
    Svg -> svg
    Math -> math
    Form -> Html.form
    Textarea -> textarea
    Button -> button
    Select -> select
    Option -> option 
    Section -> section
    Nav -> nav
    Article -> article
    Aside -> aside
    Header -> header
    Footer -> footer
    Address -> address
    Main -> main'
    Body -> body
    Figure -> figure
    Figcaption -> figcaption
    Table -> table
    Caption -> caption
    Colgroup -> colgroup
    Col -> col
    Tbody -> tbody
    Thead -> thead
    Tfoot -> tfoot
    Tr -> tr
    Td -> td
    Th -> th
    Markdown s -> 
      (\_ _-> 
        toHtmlWith 
          { githubFlavored =
            Just { tables = True, breaks = False }
          , sanitize = True
          , defaultHighlighting = Nothing
          , smartypants = False
          } [style [("white-space", "pre")]] s)
    CssTag css -> 
      (\ _ _ -> Html.node "style"
                   [ property "textContent" <| string css
                   , property "type" <| string "text/css"
                   ]
                   [])


-------------------------------------------------------------------------------

type Attr = 
   Class String
 | Id String
 | Style (List (String, String))
 | Href String
 | Title String
 | Hidden String
 | Type String
 | Value String
 | DefaultValue String
 | Checked String 
 | Placeholder String
 | Selected String 
 | Accept String
 | AcceptCharset String
 | Action String
 | Autocomplete String 
 | Autofocus String
 | Autosave String
 | Disabled String
 | Enctype String 
 | Formaction String
 | List' String  
 | Maxlength String
 | Minlength String
 | Method String
 | Multiple String
 | Name String
 | Novalidate String 
 | Pattern String
 | Readonly String
 | Required String
 | Size String
 | For String
 | Form' String
 | Max String
 | Min String 
 | Step String
 | Cols String
 | Rows String
 | Wrap String
 | Target String
 | Download String
 | DownloadAs String
 | Hreflang String
 | Media String


attrnames =
  Dict.fromList 
           [("class",Class)
           ,("id",Id)
           ,("href",Href)
           ,("title", Title)
           ,("hidden", Hidden)
           ,("type",Type)
           ,("value",Value)
           ,("defaultValue",DefaultValue )
           ,("checked",Checked )
           ,("placeholder",Placeholder)
           ,("selected",Selected )
           ,("accept",Accept )
           ,("acceptCharset",AcceptCharset)
           ,("action",Action)
           ,("autocomplete",Autocomplete)
           ,("autofocus",Autofocus)
           ,("autosave",Autosave )
           ,("disabled",Disabled )
           ,("enctype",Enctype)
           ,("formaction",Formaction)
           ,("list'",List')
           ,("maxlength",Maxlength)
           ,("minlength",Minlength)
           ,("method",Method)
           ,("multiple",Multiple)
           ,("name",Name)
           ,("novalidate",Novalidate)
           ,("pattern",Pattern)
           ,("readonly",Readonly)
           ,("required",Required)
           ,("size",Size)
           ,("for",For)
           ,("form'",Form')
           ,("max",Max)
           ,("min",Min)
           ,("step",Step)
           ,("cols",Cols)
           ,("rows",Rows)
           ,("wrap",Wrap)
           ,("target",Target)
           ,("download",Download)
           ,("downloadAs",DownloadAs)
           ,("hreflang",Hreflang)
           ,("media",Media)
           ]

capitalize s = 
  case String.uncons s of 
    Nothing -> ""
    Just (c,cs) -> String.cons (Char.toUpper c) cs

toAttr a = 
  case a of 
    Class s -> class s
    Id s    -> id s
    Href s  -> href s
    Style xs -> style xs
    Title s -> title s
    Hidden s -> hidden (toBool s)
    Type s -> type' s
    Value s -> value s
    DefaultValue s -> defaultValue s
    Checked s -> checked (toBool s)
    Placeholder s -> placeholder s
    Selected s -> selected (toBool s)
    Accept s -> accept s
    AcceptCharset s -> acceptCharset s
    Action s -> action s
    Autocomplete s -> autocomplete (toBool s) 
    Autofocus s -> autofocus (toBool s)
    Autosave s -> autosave s
    Disabled s -> disabled (toBool s)
    Enctype s -> enctype s 
    Formaction s -> formaction s
    List' s -> list s 
    Maxlength s -> maxlength (toInts s)
    Minlength s -> minlength (toInts s)
    Method s -> method s
    Multiple s -> multiple (toBool s)
    Name s -> name s
    Novalidate s -> novalidate (toBool s)
    Pattern s -> pattern s
    Readonly s -> readonly (toBool s)
    Required s -> required (toBool s)
    Size s -> size (toInts s)
    For s -> for s
    Form' s -> Html.Attributes.form s
    Max s -> Html.Attributes.max s
    Min s -> Html.Attributes.min s
    Step s -> step s
    Cols s -> cols (toInts s)
    Rows s -> rows (toInts s)
    Wrap s -> wrap s
    Target s -> target s
    Download s -> download (toBool s)
    DownloadAs s -> downloadAs s
    Hreflang s -> hreflang s
    Media s -> media s

toBool s = 
  if (s == "True" || s == "true")
  then True
  else False

toInts s = 
  case String.toInt s of 
    Err s -> -1
    Ok n  -> n
-------------------------------------------------------------------------------- 

splitAttr : Attr -> (String, String)
splitAttr attr = 
  let 
  s = String.toList (toString attr)

  f buff xs = 
    case xs of 
      [] -> (String.fromList (List.reverse buff),"")
      (x::xs) ->
        if (x == ' ')
        then (String.fromList (List.reverse buff), String.fromList xs)
        else f (x :: buff) xs
  in f [] s 
