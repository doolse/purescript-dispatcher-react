module Examples.Lifecycle where

import Prelude

import Dispatcher (action)
import Dispatcher.React (modifyState, propsRenderer, renderer)
import Effect (Effect)
import Effect.Console (log)
import React (ReactClass, ReactElement, component, createLeafElement, unsafeCreateLeafElement)
import React.DOM (button, div', h1', text)
import React.DOM.Props (onClick)

data Action = Init | Destroy

lifecycleComponent :: ReactClass {onClick :: Unit -> Effect Unit}
lifecycleComponent = component "Lifecycle" spec
  where 
  spec this = pure $ {render: propsRenderer render this}
    where 
    eval Init = log "I am alive"
    eval Destroy = log "I am dead"

    render p = div' [
          h1' [ text "I am alive" ]
        , button [onClick $ \_ -> p.onClick unit] [ text "Click to kill me"]
    ]

lifecycleParent :: ReactElement
lifecycleParent = unsafeCreateLeafElement (component "LifecycleParent" spec) {} where
  spec this = pure $ {state: {show:true}, render: renderer render this}
    where 
    d = action this
    render {state: {show}} =
      if show
      then createLeafElement lifecycleComponent {onClick: \_ -> d $ modifyState _ {show=false}}
      else div' [ text "You killed it. Check the console" ]
