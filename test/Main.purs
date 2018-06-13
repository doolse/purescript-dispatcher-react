module Test.Main (main) where

import Prelude

import Components.TaskList (taskList)
import Data.Maybe (fromJust)
import Effect (Effect)
import Examples.Ajax (ajax)
import Examples.Lifecycle (lifecycleParent)
import Examples.Toggler (toggler)
import Partial.Unsafe (unsafePartial)
import ReactDOM as RDOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
-- | The main method creates the task list component, and renders it to the document body.
main :: Effect Unit
main = void do
  document <- window >>= document
  let runExample elem contName = do
        container <- unsafePartial (fromJust <$> querySelector (QuerySelector contName) (toParentNode document))
        RDOM.render elem container
  _ <- runExample (ajax {title:"Raiders of the Lost Ark"}) "#container"
  _ <- runExample taskList "#todocontainer"
  _ <- runExample (toggler) "#togglercontainer"
  runExample (lifecycleParent) "#lifecyclecontainer"
