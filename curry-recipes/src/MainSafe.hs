{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Simple
import qualified Data.Text as T
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))
import CurryRecipes.Database (Recipe (..), database)
import Web.PathPieces

-- für Markdown
import Data.Default (def)
import Text.Markdown (markdown)
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = do
  db <- database
  runSpock 8080 $ spockT id $ do
    get "/" $
      html $ mconcat
        [ "<h1>Alle Curries</h1>"
        , showRecipes db
        , "<p><a href='/search/thai/59'>Thai-Curries, die in < 1h fertig sind</a></p>"
        , "<p><a href='/search/indian/300'>Indische Curries</a></p>"
        ]
    get "recipes/:rid" $ do
      rid <- param' "rid"
      case filter (\c -> rid == recipeId c) db of
        [] -> text "404"
        (c:_) -> html $ mconcat
          [ "<h1>" <> recipeName c <> "</h1>"
          , T.toStrict $ renderHtml $ markdown def $ T.fromStrict $ recipeInstructions c
          ]
    get "search/:category/:minutes" $ do
      cat <- param' "category"
      minutes <- param' "minutes"
      let matches c = recipeMinutesNeeded c <= minutes && recipeCategory c == cat
          recipes = filter matches db
      html $ showRecipes recipes
  where
    showRecipes cs =
      mconcat
        [ "<ul>"
        , mconcat $ map (\c -> "<li>" <> recipeLink c <> "</li>") cs
        , "</ul>"
        ]
    recipeLink c = "<a href='/recipes/" <> toPathPiece (recipeId c) <> "'>" <> recipeName c <>"</a>"
