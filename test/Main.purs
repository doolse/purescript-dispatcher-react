module Test.Main (main) where

import Prelude
import ReactDOM as RDOM
import Components.TaskList (taskListClass)
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Examples.Ajax (ajax)
import Examples.Lifecycle (lifecycleParent)
import Examples.Toggler (toggler)
import Partial.Unsafe (unsafePartial)
import React (createFactory)

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = taskListClass
  document <- DOM.window >>= DOM.document
  let runExample elem contName = do
        container <- unsafePartial (fromJust <<< toMaybe <$> DOM.querySelector contName (DOM.htmlDocumentToParentNode document))
        RDOM.render elem container
  runExample (ajax {title:"Raiders of the Lost Ark"}) "#container"
  runExample (createFactory taskListClass unit) "#todocontainer"
  runExample toggler "#togglercontainer"
  runExample lifecycleParent "#lifecyclecontainer"
