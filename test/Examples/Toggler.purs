module Examples.Toggler where

import Prelude
import Dispatcher (DispatchEff(..), effEval)
import Dispatcher.React (createComponent, modifyState)
import React (ReactElement, createFactory)
import React.DOM (button, div', h1', text)
import React.DOM.Props (onClick)

data Action = ToggleState

toggler :: ReactElement
toggler = createFactory (createComponent { on: false } render (effEval eval)) unit
  where

  render state (DispatchEff d) =
    div'
      [ h1'
          [ text "Toggle Button" ]
      , button
          [ onClick $ d \_ -> ToggleState ]
          [ text (if state.on then "On" else "Off") ]
      ]
  eval ToggleState = modifyState (\state -> { on: not state.on })
