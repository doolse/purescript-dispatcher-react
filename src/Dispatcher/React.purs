module Dispatcher.React (
  getProps
, getState
, getRefs
, modifyState
, unsafeWithRef
, unsafeWithRef'
, emptyHandler
, execHandler
, createComponent
, createComponent'
, createLifecycleComponent
, createLifecycleComponent'
, didMount
, willUnmount
, ReactState(..)
, ReactProps(..)
, ReactChildren(..)
, ReactReaderT
, ReactLifecycle
, class ReactSpecCreator
, class ReactInitialState
, class ReactRender
, createSpec
, createInitialState
, createRenderer
) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.RWS (RWS, ask, execRWS, modify)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Trans.Class (lift)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Dispatcher (class Dispatchable, class FromContext, dispatch, fromContext)
import React (GetInitialState, ReactClass, ReactElement, ReactRefs, ReactSpec, ReactThis, Read, ReadOnly, ReadWrite, Refs, Render, createClass, getChildren, readState, spec', transformState)
import React (ReactState, ReactProps, getProps, getRefs) as R
import Type.Equality (class TypeEquals, to)

-- | The type of all React action handling functions
type ReactReaderT props state m a = ReaderT (ReactThis props state) m a

-- | The type of React lifecycle methods - Reader Writer State (RWS) with the eval being the Reader type and the ReactSpec being the State type.
type ReactLifecycle props state eval eff = RWS eval Unit (ReactSpec props state ReactElement eff) Unit

reactReaderT :: forall props state m a. (ReactThis props state -> m a) -> ReactReaderT props state m a
reactReaderT = ReaderT

-- | Get the React properties
getProps :: forall m props state eff. MonadEff ( props :: R.ReactProps | eff ) m => ReactReaderT props state m props
getProps =  reactReaderT (liftEff <<< R.getProps)

-- | Get the React state
getState :: forall m props state access eff. MonadEff ( state :: R.ReactState (read::Read|access) | eff ) m => ReactReaderT props state m state
getState = reactReaderT (liftEff <<< readState)

-- | Modify the React state
modifyState :: forall m props state eff. MonadEff ( state :: R.ReactState ReadWrite | eff ) m => (state -> state) -> ReactReaderT props state m Unit
modifyState f = reactReaderT (\this -> liftEff $ transformState this f)

-- | Get the React refs
getRefs :: forall eff props state m. (MonadEff (refs :: ReactRefs ReadOnly|eff) m) => ReactReaderT props state m Refs
getRefs = reactReaderT (liftEff <<< R.getRefs)

foreign import mapRef :: forall a ref. Fn4 (ref -> a) a Refs String a

-- | Run an effect with on the object pointed to by the ref string. (or do nothing if it doesn't exist)
unsafeWithRef :: forall props state ref eff m. (MonadEff (refs::ReactRefs ReadOnly|eff) m) =>
  (ref -> Eff (refs::ReactRefs ReadOnly|eff) Unit)
  -> String
  -> ReactReaderT props state m Unit
unsafeWithRef f = void <<< unsafeWithRef' f

unsafeWithRef' :: forall props state ref eff m a. (MonadEff (refs::ReactRefs ReadOnly|eff) m) =>
  (ref -> Eff (refs::ReactRefs ReadOnly|eff) a)
  -> String
  -> ReactReaderT props state m (Maybe a)
unsafeWithRef' f s = do
  refs <- getRefs
  liftEff $ runFn4 mapRef (f >>> map Just) (pure Nothing) refs s

-- | an empty event handler
emptyHandler :: forall a m. (Applicative m) => a -> m Unit
emptyHandler = const $ pure unit

-- | Run an Eff event handler
execHandler :: forall eff eff2 m a props state. MonadEff eff m => Eff eff2 a -> ReactReaderT props state m a
execHandler e = lift $ liftEff $ unsafeCoerceEff e

class ReactSpecCreator eval initialstate render props state eff | eval initialstate render -> props state eff where
  -- | Given an action evaluator, an initial state, and a render function, return a `ReactSpec`.
  createSpec :: eval -> initialstate -> (state -> render) -> ReactSpec props state ReactElement eff

class ReactInitialState eval initialstate props state eff | initialstate -> state where
  createInitialState :: eval -> initialstate -> GetInitialState props state eff

class ReactRender eval renderer props state eff | renderer -> eff where
  createRenderer :: eval -> renderer -> Render props state ReactElement eff

-- | Create a ReactClass with the given state, renderer and action evaluator
createComponent :: forall props state state2 eval renderer eff. TypeEquals state2 state => ReactSpecCreator eval (ReactState state) renderer props state eff
  => state2 -> (state -> renderer) -> eval -> ReactClass props
createComponent state = createComponent' (ReactState $ to state :: state)

-- | Create a ReactClass with the given initialstate function, renderer and action evaluator
-- | The initialstate function must return it's state value wrapped in the `ReactState` newtype
createComponent' :: forall props state eval initialstate renderer eff. ReactSpecCreator eval initialstate renderer props state eff
  => initialstate -> (state -> renderer) -> eval -> ReactClass props
createComponent' = createLifecycleComponent' (pure unit)

-- | Create a ReactClass with the given lifecycle, state, renderer and action evaluator
createLifecycleComponent :: forall props state state2 eval renderer eff. TypeEquals state2 state => ReactSpecCreator eval (ReactState state) renderer props state eff
  => ReactLifecycle props state eval eff -> state2 -> (state -> renderer) -> eval -> ReactClass props
createLifecycleComponent specf state = createLifecycleComponent' specf (ReactState $ to state :: state)

-- | Create a ReactClass with the given lifecycle, initialstate function, renderer and action evaluator
-- | The initialstate function must return it's state value wrapped in the `ReactState` newtype
createLifecycleComponent' :: forall props state initialstate eval renderer eff. ReactSpecCreator eval initialstate renderer props state eff
  => (RWS eval Unit (ReactSpec props state ReactElement eff) Unit) ->
   initialstate -> (state -> renderer) -> eval -> ReactClass props
createLifecycleComponent' f state renderer eval = createClass $ fst $ execRWS f eval (createSpec eval state renderer)

-- | A ReactLifecycle for dispatching the `componentDidMount` React lifecycle function
didMount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (Eff ( props :: R.ReactProps, state :: R.ReactState ReadWrite, refs :: ReactRefs ReadOnly | eff) Unit)
  => action -> ReactLifecycle props state eval eff
didMount a = ask >>= \e -> modify _ { componentDidMount = flip (dispatch e) a }

-- | A ReactLifecycle for dispatching the `componentWillUnmount` React lifecycle function
willUnmount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (Eff (props :: R.ReactProps, state :: R.ReactState ReadOnly, refs :: ReactRefs ReadOnly | eff) Unit)
  => action -> ReactLifecycle props state eval eff
willUnmount a = ask >>= \e -> modify _ { componentWillUnmount = flip (dispatch e) a }

instance defaultCreateSpec :: (ReactInitialState eval initialstate props state eff, ReactRender eval renderer props state eff)
  => ReactSpecCreator eval initialstate renderer props state eff where
  createSpec eval initial renderer = spec' (createInitialState eval initial) \this -> do
    s <- readState this
    (createRenderer eval $ renderer s) this

instance stateGetInitialState :: ReactInitialState eval (ReactState state) props state eff where
  createInitialState eval (ReactState s) = const $ pure s

instance fromContextIS :: (FromContext eval (ReactThis props state) r Eff (props :: R.ReactProps, refs::ReactRefs (), state::R.ReactState ()|eff)
  , ReactInitialState eval next props state eff)
  => ReactInitialState eval (r -> next) props state eff where
  createInitialState eval initial = \this -> do
    r <- fromContext eval this
    createInitialState eval (initial r) this

instance renderReactElem :: ReactRender eval ReactElement props state eff where
  createRenderer _ = const <<< pure

instance fromContextRR :: (
    ReactRender eval next props state eff
  , FromContext eval (ReactThis props state) r Eff (props :: R.ReactProps, refs::ReactRefs (), state::R.ReactState (read::Read)|eff)
  )
  => ReactRender eval (r -> next) props state eff where
  createRenderer eval f = \this -> do
    r <- fromContext eval this
    createRenderer eval (f r) this

-- | A newtype to signify the React State
-- | Used to denote the state returned from an initial state function
newtype ReactState s = ReactState s

-- | A newtype representing the React Props
-- | Useful in renderers or initial state functions
newtype ReactProps p = ReactProps p

-- | A newtype representing the React children array
-- | Useful in renderers
newtype ReactChildren = ReactChildren (Array ReactElement)

instance stateFromContext :: TypeEquals state state2 => FromContext eval (ReactThis props state) (ReactState state2) Eff (state::R.ReactState (read::Read|access)|eff) where
  fromContext _ = map (ReactState <<< to) <<< readState

instance propsFromContext :: TypeEquals props props2 => FromContext eval (ReactThis props state) (ReactProps props2) Eff (props::R.ReactProps|eff) where
  fromContext _ = map (ReactProps <<< to) <<< R.getProps

instance childrenFromContext :: FromContext eval (ReactThis props state) ReactChildren Eff (props::R.ReactProps|eff) where
  fromContext _ = map ReactChildren <<< getChildren
