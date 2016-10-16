module Pad exposing (..)

initCss = 
 """.main body{
 background-color: lightgrey;
}

.main h1 {
 border-style: solid;
}

 """

testinput = 
  """ div [ class "main"
    ]
    [ header []
             [ h1 []
                  [ text "A great page"
                  ]
             ]
    , body []
           [ form []
                  [ textarea []
                             [ text "placeholder"
                             ]
                  , button []
                           [ text "press here!"
                           ]
                  ]
           , a [ href "http://www.google.com"
               ]
               [ text "the answer to everything"
               ]
           , table []
                   [ th []
                        [ text "table header"
                        ]
                   , tr []
                        [ td []
                             [ text "case 1"
                             ]
                        , td []
                             [ text "case 2"
                             ]
                        , td []
                             [ text "case 3"
                             ]
                        , td []
                             [ text "case 4"
                             ]
                        ]
                   , tr []
                        [ td []
                             [ text "case 5"
                             ]
                        , td []
                             [ text "case 6"
                             ]
                        , td []
                             [ text "case 7"
                             ]
                        , td []
                             [ text "case 8"
                             ]
                        ]
                   ]
           ]
    , br [] []
    , markdown 
"| First Header  | Second Header |
 | ------------- | ------------- |
 | Content Cell  | Content Cell  |
 | Content Cell  | Content Cell  |
        
 - [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
 - [x] list syntax required (any unordered or ordered list supported)
 - [x] this is a complete item
 - [ ] this is an incomplete item
"
    , footer []
             [ text "this is the end"
             ]
    ]

  """
testinput2 = 
   """ textarea [] [text "hello"]

  """

testinput3 = 
   """ markdown 
   "# This is Markdown

[Markdown](http://daringfireball.net/projects/markdown/) lets you
write content in a really natural way.

  * You can have lists, like this one
  * Make things **bold** or *italic*
  * Embed snippets of `code`
  * Create [links](/)
  * ...

The [elm-markdown][] package parses all this content, allowing you
to easily generate blocks of `Element` or `Html`.

[elm-markdown]: http://package.elm-lang.org/packages/evancz/elm-markdown/latest
"   """

testinput4 = 
  """ markdown 
  "test

   | First Header  | Second Header |
   | ------------- | ------------- |
   | Content Cell  | Content Cell  |
   | Content Cell  | Content Cell  |
  - [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
  - [x] list syntax required (any unordered or ordered list supported)
  - [x] this is a complete item
  - [ ] this is an incomplete item
  "
  
  """