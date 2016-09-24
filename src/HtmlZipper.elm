module HtmlZipper exposing (..)

import TagAttr exposing (TagName,Attr, splitAttr)
import Dict exposing (get)
import String exposing (toLower, dropRight, dropLeft)

---------------------------------------------------------------------------

type Tree a = Node a (List (Tree a))

type Context a = Context a (List (Tree a)) (List (Tree a))

type ZipTree a = ZipTree ( Tree a, List (Context a))

zipUp : ZipTree a -> Maybe (ZipTree a)
zipUp (ZipTree (fTree , ctxs)) =
  case ctxs of 
    [] -> Nothing
    ((Context parent ls rs) :: bs) ->  
      Just (ZipTree ( Node parent (ls ++ [fTree] ++ rs), bs))  


zipDown : ( Tree a -> Bool ) -> ZipTree a -> Maybe (ZipTree a)
zipDown p (ZipTree (Node fVal ts, ctxs)) =
  case ts of 
    [] -> Nothing
    ts -> 
      let (l,r) = break p ts
      in case r of
          [] -> Nothing
          (t :: ts') -> Just (ZipTree (t,(Context fVal l ts') :: ctxs))

zipRight : ZipTree a -> Maybe (ZipTree a)
zipRight (ZipTree (fTree , ctxs)) =
  case ctxs of 
    [] -> Nothing
    ((Context parent ls rs) :: bs) -> 
      case rs of
        [] -> Nothing
        (t::ts) -> 
          Just (ZipTree (t, (Context parent (ls ++ [fTree]) ts) :: ctxs))

zipLeft : ZipTree a -> Maybe (ZipTree a)
zipLeft (ZipTree (fTree , ctxs)) =
  case ctxs of 
    [] -> Nothing
    ((Context parent ls rs) :: bs) -> 
      case (List.reverse ls) of
        [] -> Nothing
        (t::ts) -> 
          Just 
            (ZipTree 
              ( t
              , (Context parent (List.reverse ts) (fTree :: rs)) :: ctxs)
            )

cd : (List b) -> ( a -> b ) -> ZipTree a -> Maybe (ZipTree a)
cd path f zt = 
  case path of 
    [] -> Nothing
    
    (p :: []) ->
      let g (Node val _) = (f val == p)
      in (zipDown g zt) 

    (p :: ps) ->
      let g (Node val _) = (f val == p)
      in case (zipDown g zt) of 
          Nothing -> Nothing
          Just zt'-> cd ps f zt'  

-------------------------------------------------------------------------------

type alias HTML = Tree Tag
type alias Tag = 
  { tagname : TagName
  , path : List TagName
  , attr : List Attr
  }



htmlToString : HTML -> String
htmlToString html =
  let
  spacer indent = 
    if indent == 0
    then ""
    else " " ++ (spacer (indent - 1))

  getT tagname = 
    toLower (toString tagname)

  getA attrname = 
      toLower (toString attrname)
  
  attrListToString ats =  
    List.map (\a -> let (an,payload) = splitAttr a
                    in (trimQuot (getA an)) ++ " "++ payload ) ats


  helper indent (Node {tagname, path, attr} childs) =
    let tn = getT tagname
        buff = spacer (indent + off)
        off  =  (String.length tn)
        atList = attrListToString attr
        tglist = (List.map (helper (indent + off +  3)) childs)
    
    in if String.startsWith "text" tn
       then tn
       else let 
                atln = if atList == []
                       then " []"
                       else " [ " ++ (String.join ("\n" ++ buff ++ " , ") atList) ++ ("\n" ++ buff ++ " ]")
                tgln = if tglist == []
                       then " []"
                       else " [ " ++ (String.join ("\n" ++ buff ++ " , ") tglist) ++ ("\n" ++ buff ++ " ]")
            in
            tn ++ atln ++ "\n" ++
            buff ++ tgln 

  in helper 0 html

-------------------------------------------------------------------------------

trimQuot s = (dropRight 1 (dropLeft 1 s))

break : (a -> Bool) -> List a -> (List a, List a)
break p xs = 
  let 
    helper ys left = 
      case ys of 
        [] -> (left,[])
        (y::ys) -> 
          if p y 
          then (List.reverse left, y :: ys)
          else helper ys (y :: left)
  in helper xs []