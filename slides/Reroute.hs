{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

-- Teil zwei: Routen

module Reroute where

import HVect
import qualified Data.Text as T
import Web.PathPieces -- cabal install path-pieces
import Data.String (IsString (..))

data Path (as :: [*]) where
  Empty :: Path '[] -- the empty path
  StaticCons :: T.Text -> Path as -> Path as -- append a static path piece to path
  VarCons :: PathPiece a => Path as -> Path (a ': as) -- append a param to path

-- | A route parameter
var :: PathPiece a => Path (a ': '[])
var = VarCons Empty

type Var a = Path (a ': '[])

-- | A static route piece
static :: String -> Path '[]
static s = StaticCons (T.pack s) Empty
  --let pieces = filter (not . T.null) $ T.splitOn "/" $ T.pack s
  --in foldr StaticCons Empty pieces

instance (a ~ '[]) => IsString (Path a) where
    fromString = static

-- Falsch wäre:
-- instance IsString (Path '[]) where

-- Zum Beispiel: Bei ("foo" <//> "bar") :: Path '[]
-- kann der Typchecker *nicht* folgern, dass "foo" :: Path '[] haben muss.
-- Er schafft nur "foo" :: IsString (Path a) => Path a zu inferieren.
-- Dann kommt die Instanz und sagt dem Compiler: "Damit (Path a) die Klasse IsString 
-- implementiert, muss a ~ '[] gelten. Also setze a := '[]".
-- Die falsche Instanz sagt lediglich: Wenn a = '[], dann implementiert (Path a)
-- die Klasse IsString.

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[]
root = Empty

(<//>) :: Path as -> Path bs -> Path (Append as bs)
(<//>) Empty xs = xs
(<//>) (StaticCons pathPiece xs) ys = (StaticCons pathPiece (xs <//> ys))
(<//>) (VarCons xs) ys = (VarCons (xs <//> ys))

renderRoute' :: Path as -> HVect as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
    ( pathPiece : renderRoute' pathXs paramXs )
renderRoute' (VarCons pathXs) (HCons val paramXs) =
    ( toPathPiece val : renderRoute' pathXs paramXs)
renderRoute' _ _ = error "This will never happen."

renderRoute :: HasRep as => Path as -> HVectElim as T.Text
renderRoute p = hVectCurry (T.intercalate "/" . renderRoute' p)

-- Man kann
--   getRep :: Path as -> Rep as
-- schreiben und damit sogar die Voraussetzung `HasRep as` fallen lassen.

matchRoute' :: Path as -> [T.Text] -> Maybe (HVect as)
matchRoute' Empty [] = Just HNil
matchRoute' Empty _  = Nothing
matchRoute' (StaticCons p ps) [] = Nothing
matchRoute' (StaticCons p ps) (p':ps')
  | p == p' = matchRoute' ps ps'
  | otherwise = Nothing
matchRoute' (VarCons ps) (p':ps') = HCons <$> fromPathPiece p' <*> matchRoute' ps ps'

matchRoute :: Path as -> T.Text -> HVectElim as a -> Maybe a
matchRoute path url elim = hVectUncurry elim <$> matchRoute' path (T.splitOn "/" url)

-- Example
type BlogPostId = Int

blogPostR :: Path '[BlogPostId]
blogPostR = "blog" <//> var
