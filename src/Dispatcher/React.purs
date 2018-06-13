module Dispatcher.React (
  getProps
, getState
, modifyState
, ReactReaderT
, renderer
, propsRenderer
, stateRenderer
, emptyHandler
, saveRef
, withRef
) where

import Prelude

import Control.Monad.Reader (ReaderT(ReaderT))
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, read, write)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import React (ReactElement, ReactThis)
import React as R

-- | The type of all React action handling functions
type ReactReaderT props state m a = ReaderT (ReactThis props state) m a

reactReaderT :: forall props state m a. (ReactThis props state -> m a) -> ReactReaderT props state m a
reactReaderT = ReaderT

-- | Get the React properties
getProps :: forall m props state. MonadEffect m => ReactReaderT props state m props
getProps =  reactReaderT (liftEffect <<< R.getProps)

-- | Get the React state
getState :: forall m props state. MonadEffect m => ReactReaderT props state m state
getState = reactReaderT (liftEffect <<< R.getState)

-- | an empty event handler
emptyHandler :: forall a m. (Applicative m) => a -> m Unit
emptyHandler = const $ pure unit

-- | Modify the React state
modifyState :: forall m props state. MonadEffect m => (state -> state) -> ReactReaderT props state m Unit
modifyState f = reactReaderT (\this -> liftEffect $ R.modifyState this f)

-- | Create a render function using just the props
propsRenderer :: forall props state. (props -> ReactElement) -> ReactThis props state -> Effect ReactElement
propsRenderer f this = f <$> R.getProps this 

-- | Create a render function using just the state
stateRenderer :: forall props state. (state -> ReactElement) -> ReactThis props state -> Effect ReactElement
stateRenderer f this = f <$> R.getState this

-- | Create a render function using both props and state
renderer :: forall props state. ({state :: state, props :: props} -> ReactElement) -> ReactThis props state -> Effect ReactElement
renderer f this = do 
  props <- R.getProps this
  state <- R.getState this 
  pure $ f {state, props}

-- | Save a React reference (DOM or Component) into a purescript mutable referance
saveRef :: forall ref. Ref (Maybe ref) -> EffectFn1 (Nullable ref) Unit
saveRef ref = mkEffectFn1 $ toMaybe >>> flip write ref

-- | Run an effect on the mutable reference if it's available
withRef :: forall ref m. MonadEffect m => Ref (Maybe ref) -> (ref -> m Unit) -> m Unit
withRef ref f = (liftEffect $ read ref) >>= maybe (pure unit) f
