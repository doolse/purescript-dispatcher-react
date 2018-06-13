module Dispatcher (
  affAction
, action
, Dispatch1(..)
, mkDispatch1
) where

import Prelude

import Control.Monad.Reader (ReaderT, mapReaderT, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

-- | Create an action dispatcher with any Functor and context (e.g. Effect & ReactThis)
action :: forall m r a. Functor m => r -> (ReaderT r m a -> m Unit)
action this = flip runReaderT this <<< void

-- | Create an action dispatcher for Aff and any given the context (e.g. ReactThis)
affAction :: forall r a. r -> ReaderT r Aff a -> Effect Unit
affAction this = flip runReaderT this <<< mapReaderT launchAff_

newtype Dispatch1 m a = Dispatch1 (forall event. (event -> a) -> m event Unit)

-- | Make a EffectFn1 action dispatcher given an effect dispatcher
mkDispatch1 :: forall a. (a -> Effect Unit) -> Dispatch1 EffectFn1 a
mkDispatch1 eval = Dispatch1 (\f -> mkEffectFn1 $ \a -> eval (f a))

