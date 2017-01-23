module Examples.Lifecycle where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Dispatcher (DispatchEff(..), effEval)
import Dispatcher.React (ReactProps(..), createComponent, createLifecycleComponent, didMount, modifyState, willUnmount)
import React (ReactElement, createFactory)
import React.DOM (button, div', h1', text)
import React.DOM.Props (onClick)

data Action = Init | Destroy

lifecycleComponent :: forall eff. {onClick :: Unit -> Eff eff Unit} -> ReactElement
lifecycleComponent = createFactory (createLifecycleComponent (didMount Init *> willUnmount Destroy) {} render eval) where
  render s (ReactProps p) (DispatchEff d) = div' [
        h1' [ text "I am alive" ]
      , button [onClick $ d \_ -> p.onClick unit] [ text "Click to kill me"]
    ]
  eval Init = log "I am alive"
  eval Destroy = log "I am dead"


lifecycleParent :: ReactElement
lifecycleParent = createFactory (createComponent {show:true} render unit) unit where
  render {show} (DispatchEff d) =
    if show
    then lifecycleComponent {onClick: d $ effEval \_ ->  modifyState _ {show=false}}
    else div' [ text "You killed it. Check the console" ]
