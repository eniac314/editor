module Pad exposing (..)


initCss =
    """.main body {
  background-color: lightgrey;
}

.main h1:hover {
  border-style: solid;
}

.other h1  {
  padding: 2em;
}

#AGreatDiv div {
  width: 20px;
  font-size: 0.2em;
  color: rgb(125, 15, 21);
}

 """


initCss2 =
    """#bourg1{
  float: left;
  margin-right: 1em;
}

.toClear{
  clear: both;
}


#bourg2, #bourg3, #bourg4{
 margin-right: 1em;
 margin-bottom: 1em;
 max-width: 29%;
 display: inline-block;
}

#bourg4{
  max-width: 32.76%;
}

h4 {
  margin-top: 0.5em;
  text-transform: uppercase;
  font-size: 100%;
  color:  #432e2a;
  text-align: center;
  background-color: peru;
}

.subContainerData{
 margin: auto;
 padding-top: 3em;
 padding-bottom: 3em;
 width: 90%;
}

.subContainerData a {
  text-decoration: none;
  color: teal;
  padding:0 0.1em;
}
.subContainerData a:hover {
  color: white;
  background-color: teal;
  border-radius: 2px;
}
.subContainerData table
{
  width:40%;
}

.subContainerData table p
{ padding-left: 1em;
  padding-right: 1em;
  padding-top: 0em;
  padding-bottom: 0em;
  text-indent: 0;
  margin: 0.5em;
}

.subContainerData td, .subContainerData th{
  border-bottom: 1px solid gray;
  background-color: lightgrey;
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


testinput5 =
    """div [class "subContainerData"] [
  h4 [] [text "Le bourg de murol"]
  , img [src "/images/2 Murol, le bourg.jpg", id "bourg1"] []
                , p  [] [text "Le bourg de Murol est implanté dans un
                               écrin de verdure à 850 mètres d'altitude, dans
                               la vallée de la Couze Chambon, sur le
                               versant Est du massif du Sancy."]
                , p [] [ text "Enchâssé entre le volcan boisé du "
                       , a [href "/DécouvrirMurol.html?bloc=Le volcan du Tartaret"]
                           [text "Tartaret"]
                       , text " le promontoire du "
                       , a [href "/DécouvrirMurol.html?bloc=01"]
                           [text "château de Murol"]
                       , text " et le puy
                             de Bessolles, le village vous ravira par ses
                             sites remarquables et pittoresques. "
                       ]

                , p []
                       [ text "Au pied du château, découvrez le parc
                              arboré du Prélong où se trouvent le "
                       , a [ target "_blank", href "http://www.musee-murol.fr/fr"]
                           [ text "musée des Peintres de l’Ecole de Murols"]
                       , text " et le musée archéologique. Depuis le bourg, rejoignez
                               la plage de Murol, au bord du "
                       , a [href "/DécouvrirMurol.html?bloc=Le lac Chambon"]
                           [text "lac Chambon"]
                       , text " en empruntant "
                       , a [href "/DécouvrirMurol.html?bloc=La voie verte"]
                           [text "la voie verte"]
                       , text "."
                       ]
                , p [] [ text "Le bourg de Murol offre de nombreux "
                       , a [href "/Commerces.html"] [text "commerces et services"]
                       , text ". Le marché hebdomadaire a lieu chaque
                              mercredi matin. "
                       ]
                , p [class "toClear"] [ text "Murol est animé tout au long de l’année par de "
                       , a [href "/Animation.html"]
                           [text "grandes manifestations"]
                       , text " à destination d’un public
                               familial. Chaque dimanche de la saison estivale, participez à une visite insolite du bourg
                               en suivant Monsieur Alphonse…"
                       ]
                , img [src "/images/prelong.jpg", id "bourg2"] []
                , img [src "/images/museePeintre.jpeg", id "bourg3"] []
                , img [src "/images/bourg2.jpg", id "bourg4"] []

  ]
  """


testinput6 =
    """div [class "subContainerData"] [
  h4 [] [text "Le bourg de murol"]
  , img [src "/images/2 Murol, le bourg.jpg", id "bourg1"] []
                markdown
                "Le bourg de Murol est implanté dans un écrin de verdure à 850 mètres d'altitude, dans la vallée de la Couze Chambon, sur le versant Est du massif du Sancy.

Enchâssé entre le volcan boisé du [Tartaret](/DécouvrirMurol.html?bloc=Le volcan du Tartaret) le promontoire du [château de Murol](/DécouvrirMurol.html?bloc=01) et le puy de Bessolles, le village vous ravira par ses sites remarquables et pittoresques.

Au pied du château, découvrez le parc arboré du Prélong où se trouvent le [musée des Peintres de l’Ecole de Murols](http://www.musee-murol.fr/fr) et le musée archéologique. Depuis le bourg, rejoignez la plage de Murol, au bord du [lac Chambon](/DécouvrirMurol.html?bloc=Le lac Chambon) en empruntant la [voie verte](/DécouvrirMurol.html?bloc=La voie verte).

Le bourg de Murol offre de nombreux commerces et services. Le marché hebdomadaire a lieu chaque mercredi matin.

Murol est animé tout au long de l’année par de grandes manifestations à destination d’un public familial. Chaque dimanche de la saison estivale, participez à une visite insolite du bourg en suivant Monsieur Alphonse…

"
, br [class "toClear"] []
, br [] []

, img [src "/images/prelong.jpg", id "bourg2"] []
, img [src "/images/museePeintre.jpeg", id "bourg3"] []
, img [src "/images/bourg2.jpg", id "bourg4"] []

, markdown

"| Titre 1       | Titre 2       |
 | ------------- | ------------- |
 | Content Cell  | Content Cell  |
 | Content Cell  | Content Cell  |
"

  ]
  """
