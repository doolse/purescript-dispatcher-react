module Test.Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import CountButton.Dispatcher (useCountButton)
import DOM (DOM) as DOM
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)
import Data.Foldable (traverse_)
import Data.Nullable (toMaybe)
import React.DOM (div')
import ReactDOM (render)

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (console::CONSOLE,dom :: DOM.DOM) Unit
main = do
  document <- window >>= document
  container <- toMaybe <$> body document
  traverse_ (render (div' $ useCountButton 10 5) <<< htmlElementToElement) container
