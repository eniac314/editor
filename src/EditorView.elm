module EditorView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.CssHelpers
import EditorCss exposing (..)
import HtmlZipper exposing ( HTML
                           , HtmlZipper
                           , Path
                           , Tag
                           , Tree (..)
                           , ZipTree (..)
                           )
import Types exposing (..)
import String exposing (words)
import TagAttr exposing (TagName)
import Markdown exposing (..)

{ id, class, classList } =
    Html.CssHelpers.withNamespace "editor"

renderEditor : Model -> List (Html Msg)
renderEditor model = 
  [ renderPath (.currPath model)
  , div [ class [ Pane ]
        , id LeftPane
        ] 
        [ Html.form 
            []
            [ textarea [ onInput Store
                       , rows 15
                       , cols 45
                       , id Prompt
                       --, style [("white-space","pre")]
                       ]
                       [ case (.procString model) of 
                          Nothing -> text (.rawString model)
                          Just s  -> text s
                       ]
            , br [] []
            , button [onClick Parse, type' "reset"] [ text "Parse"]
            , button [onClick Render, type' "button"] [ text "Render"]
            , button [onClick Debug, type' "button"] [ text "Debug"]
            ]
        ]
  , div [ id RightPane
        , class [Pane]
        ]
        [ explorer model
        ]
  , renderConsole model
  , div [classList [("DebugClass",.debug model)]]
        [ text (toString (.nblines model))
        , toHtmlWith 
              { githubFlavored =
                Just { tables = True, breaks = False }
              , sanitize = True
              , defaultHighlighting = Nothing
              , smartypants = False
              } [style [("white-space", "pre")]] (.rawString model)
        , div [style [("white-space", "pre")]]
            [text <| toString (.parsedData model)]
        ]
  ]

explorer : Model -> Html Msg
explorer model = 
  let
  page = .page model
  dbug = .debug model
  
  sizeExplorer = 
    case .winSize model of
      Nothing -> (560 ,300)
      Just {width, height} -> (width//2,300)
  
  explWindow tags =
    div [ id ExplWindow]
        [ tags ]
  tags = 
    case page of 
      Nothing -> span [] []
      Just zp -> render zp

  render (ZipTree (t ,ctx)) = 
    let
     spacer indent = 
       if indent == 0
       then ""
       else " " ++ (spacer (indent - 1))
     
     colors = List.reverse ["ivory","khaki","lavender","lavenderblush"
              ,"lightcoral","lightgreen","lemonchiffon"
              ,"thistle","mediumspringgreen","lightskyblue"
              ]

     colorPicker xs = 
      case xs of
        [] -> (colorPicker colors) 
        (x::xs) -> (x,xs)

     render' n cs (Node tag xs) =
      let tn   = 
           case (.tagname tag) of 
            TagAttr.Text _ -> "Text"
            TagAttr.Markdown _ -> "Markdown"
            tn'    -> toString tn'
          pth  = .path tag

          (c,cs') = colorPicker cs
          
      in p [ class []
           , style [("margin","0.1em")]
           , onClick (GoTo pth)
           ]
           ([ text (spacer n)
            , span [class [ExplTag]
            , style [  ("background-color",c) ]
            ] 
            [text tn]
            ] 
            ++ [span [classList [("DebugClass",dbug)]] [text (toString pth)]] ++ 
            (List.map (render' (n+3) cs') xs))

    in render' 0 colors t    


  in div [ id Explorer
         , style [("white-space","pre")]
         ]
         [ explWindow tags
         , button [onClick Up, type' "reset"] [ text "Left"]
         , button [onClick Down, type' "reset"] [ text "Right"]
         , button [onClick Left, type' "reset"] [ text "Up"]
         , button [onClick Right, type' "reset"] [ text "Down"]
         ]


renderPath : Path -> Html msg
renderPath path = 
  let pathStr = 
       span [ class [Mono]
            , id PathStr
            ]
            [ text (String.join "/" 
                     (List.reverse 
                       (List.map (\(t,p) -> toString t) path)))
            ]
  in div [id Path]
         [p [] [text "Current path: ", pathStr]
         ]

renderConsole : Model -> Html msg
renderConsole model = 
  div [ id Console 
      ]
      [ div [ class [Mono] ]
            [ case (.parsedData model) of 
                 Err s -> span [class [Error]] [text s]
                 Ok r  -> text "parsing complete" 
            ]
      ]