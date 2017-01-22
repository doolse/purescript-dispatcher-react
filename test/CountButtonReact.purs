module CountButton.React where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import React (ReactElement, createClass, createFactory, getProps, readState, spec, spec', transformState)
import React.DOM (button, div', text)
import React.DOM.Props (onClick)

type CountButtonProps eff = { clicks::Int, onFinishedClicks :: Unit -> Eff eff Unit }

countButton :: forall eff. CountButtonProps eff -> ReactElement
countButton = createFactory countButtonClass
  where
  -- countButtonClass :: ReactClass (CountButtonProps eff)
  countButtonClass = createClass (spec' getInitialState render)
    where
      getInitialState this = (\p -> {clicksLeft: p.clicks}) <$> getProps this

      render this = do
        p <- getProps this
        s <- readState this
        let clicked = if s.clicksLeft == 1 then
                unsafeCoerceEff $ p.onFinishedClicks unit
              else
                transformState this \st -> st {clicksLeft = st.clicksLeft - 1}
        pure $ button [onClick \_ -> clicked]
                      [text $ "Click me " <> show s.clicksLeft <> " more times"]

useCountButton :: {clicks1 :: Int, clicks2 :: Int } -> ReactElement
useCountButton = createFactory $ createClass $ spec unit render
  where
    render this = do
      {clicks1,clicks2} <- getProps this
      pure $ div' [
          countButton {clicks:clicks1, onFinishedClicks: \_ -> logClick "Button 1" clicks1}
        , countButton {clicks:clicks2, onFinishedClicks: \_ -> logClick "Button 2" clicks2}
      ]
    logClick name count = log $ name <> " was clicked " <> show count <> " times."

useCountButton' :: {clicks1 :: Int, clicks2 :: Int } -> ReactElement
useCountButton' {clicks1,clicks2} =
  div' [
      countButton {clicks:clicks1, onFinishedClicks: \_ -> logClick "Button 1" clicks1}
    , countButton {clicks:clicks2, onFinishedClicks: \_ -> logClick "Button 2" clicks2}
  ]
  where
    logClick name count = log $ name <> " was clicked " <> show count <> " times."
