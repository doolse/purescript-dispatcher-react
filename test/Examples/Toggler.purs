module Examples.Toggler where

import Prelude

import Dispatcher (action)
import Dispatcher.React (modifyState, renderer)
import React (ReactElement, component, unsafeCreateLeafElement)
import React.DOM (button, div', h1', text)
import React.DOM.Props (onClick)

data Action = ToggleState

toggler :: ReactElement
toggler = unsafeCreateLeafElement (component "Toggler" spec) {}
  where
  spec this = 
    let eval ToggleState = modifyState (\state -> { on: not state.on })
        d = action this <<< eval
        render {state} =
            div'
            [ h1'
                [ text "Toggle Button" ]
            , button
                [ onClick $ \_ -> d ToggleState ]
                [ text (if state.on then "On" else "Off") ]
            ]
    in pure {state: { on: false }, render: renderer render this}
