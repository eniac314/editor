module HtmlZipper exposing (..)

import TagAttr exposing (TagName,Attr)

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


-------------------------------------------------------------------------------

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