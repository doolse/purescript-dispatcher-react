module CountButton.Dispatcher where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Dispatcher (DispatchEff(..), effEval)
import Dispatcher.React (ReactProps(..), ReactState(..), createComponent', execHandler, getProps, getState, modifyState)
import React (ReactElement, createFactory)
import React.DOM (button, text)
import React.DOM.Props (onClick)

type CountButtonProps eff = { clicks::Int, onFinishedClicks :: Unit -> Eff eff Unit }

data Action = Clicked

countButton :: forall eff. CountButtonProps eff -> ReactElement
countButton = createFactory $ createComponent' getInitialState render (effEval eval)
  where
    getInitialState (ReactProps p) = ReactState {clicksLeft: p.clicks}
    render s (DispatchEff d) = button [onClick $ d \_ -> Clicked]
                                      [text $ "Click me " <> show s.clicksLeft <> " more times"]
    eval Clicked = do
      {clicksLeft} <- getState
      if clicksLeft == 1
       then do
        {onFinishedClicks} <- getProps
        execHandler $ onFinishedClicks unit
       else modifyState \s -> s {clicksLeft = s.clicksLeft - 1}


useCountButton :: Int -> Int -> Array ReactElement
useCountButton clicks1 clicks2 = [
    countButton {clicks:clicks1, onFinishedClicks: \_ -> logClick "Button 1" clicks1}
  , countButton {clicks:clicks2, onFinishedClicks: \_ -> logClick "Button 2" clicks2}
  ]
  where
    logClick name count = log $ name <> " was clicked " <> show count <> " times."
