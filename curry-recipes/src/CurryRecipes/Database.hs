{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CurryRecipes.Database where

import Paths_curry_recipes
import Control.Monad (sequence)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.PathPieces

newtype RecipeId = RecipeId { getRecipeId :: Int } deriving (Show, Read, Eq, Num, PathPiece)

data RecipeCategory = Indian | Thai deriving (Show, Read, Eq)

instance PathPiece RecipeCategory where
  fromPathPiece "indian" = Just Indian
  fromPathPiece "thai" = Just Thai
  fromPathPiece _ = Nothing
  toPathPiece Indian = "indian"
  toPathPiece Thai = "thai"

data Recipe =
  Recipe { recipeId :: RecipeId
         , recipeCategory :: RecipeCategory
         , recipeName :: T.Text
         , recipeMinutesNeeded :: Int
         , recipeInstructions :: T.Text
         } deriving (Show, Read)

database :: IO [Recipe]
database = sequence
  [ Recipe 1 Thai "Leckeres vegetarisches Curry" 30 <$> readCurryFile "veg-curry.md"
  , Recipe 2 Indian "Chicken tikka masala" 100 <$> readCurryFile "chicken-tikka-masala.md"
  , Recipe 3 Thai "Massaman Curry" 60 <$> readCurryFile "veg-curry.md"
  ]
  where
    readCurryFile name = getDataFileName ("recipes/" ++ name) >>= T.readFile
