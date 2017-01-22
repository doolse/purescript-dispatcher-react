module Dispatcher.React (
  getProps
, getState
, getRefs
, modifyState
, unsafeWithRef
, emptyHandler
, execHandler
, createComponent
, createLifecycleComponent
, createComponent'
, didMount
, willUnmount
, ReactState(..)
, ReactProps(..)
, ReactChildren(..)
, ReactReaderT
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
import Data.Tuple (fst)
import Dispatcher (class Dispatchable, class FromContext, dispatch, fromContext)
import React (GetInitialState, ReactClass, ReactElement, ReactRefs, ReactSpec, ReactThis, Read, ReadOnly, ReadWrite, Refs, Render, createClass, getChildren, readState, spec', transformState)
import React (ReactState, ReactProps, getProps, getRefs) as R
import Type.Equality (class TypeEquals, to)

type ReactReaderT props state m a = ReaderT (ReactThis props state) m a

reactReaderT :: forall props state m a. (ReactThis props state -> m a) -> ReactReaderT props state m a
reactReaderT = ReaderT

getProps :: forall m props state eff. MonadEff ( props :: R.ReactProps | eff ) m => ReactReaderT props state m props
getProps =  reactReaderT (liftEff <<< R.getProps)

getState :: forall m props state eff. MonadEff ( state :: R.ReactState ReadWrite | eff ) m => ReactReaderT props state m state
getState = reactReaderT (liftEff <<< readState)

modifyState :: forall m props state eff. MonadEff ( state :: R.ReactState ReadWrite | eff ) m => (state -> state) -> ReactReaderT props state m Unit
modifyState f = reactReaderT (\this -> liftEff $ transformState this f)

getRefs :: forall eff props state m. (MonadEff (refs :: ReactRefs ReadOnly|eff) m) => ReactReaderT props state m Refs
getRefs = reactReaderT (liftEff <<< R.getRefs)

foreign import mapRef :: forall a ref. Fn4 (ref -> a) a Refs String a

unsafeWithRef :: forall props state ref eff m. (MonadEff (refs::ReactRefs ReadOnly|eff) m) =>
  (ref -> Eff (refs::ReactRefs ReadOnly|eff) Unit)
  -> String
  -> ReactReaderT props state m Unit
unsafeWithRef f s = do
  refs <- getRefs
  liftEff $ runFn4 mapRef f (pure unit) refs s

emptyHandler :: forall a m. (Applicative m) => a -> m Unit
emptyHandler = const $ pure unit

execHandler :: forall eff eff2 m a props state. MonadEff eff m => Eff eff2 a -> ReactReaderT props state m a
execHandler e = lift $ liftEff $ unsafeCoerceEff e

class ReactSpecCreator eval initialstate render props state eff | eval initialstate render -> props state eff where
  createSpec :: eval -> initialstate -> (state -> render) -> ReactSpec props state eff

class ReactInitialState eval initialstate props state eff | initialstate -> state where
  createInitialState :: eval -> initialstate -> GetInitialState props state eff

class ReactRender eval renderer props state eff where
  createRenderer :: eval -> renderer -> Render props state eff

createComponent :: forall props state eval renderer eff. ReactSpecCreator eval (ReactState state) renderer props state eff
  => state -> (state -> renderer) -> eval -> ReactClass props
createComponent state = createComponent' (ReactState state)

createComponent' :: forall props state eval initialstate renderer eff. ReactSpecCreator eval initialstate renderer props state eff
  => initialstate -> (state -> renderer) -> eval -> ReactClass props
createComponent' = createLifecycleComponent' (pure unit)

createLifecycleComponent :: forall props state eval renderer eff. ReactSpecCreator eval (ReactState state) renderer props state eff
  => (RWS eval Unit (ReactSpec props state eff) Unit) ->
   state -> (state -> renderer) -> eval -> ReactClass props
createLifecycleComponent specf state = createLifecycleComponent' specf (ReactState state)

createLifecycleComponent' :: forall props state initialstate eval renderer eff. ReactSpecCreator eval initialstate renderer props state eff
  => (RWS eval Unit (ReactSpec props state eff) Unit) ->
   initialstate -> (state -> renderer) -> eval -> ReactClass props
createLifecycleComponent' f state renderer eval = createClass $ fst $ execRWS f eval (createSpec eval state renderer)

didMount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (Eff ( props :: R.ReactProps, state :: R.ReactState ReadWrite, refs :: ReactRefs ReadOnly | eff) Unit)
  => action -> RWS eval Unit (ReactSpec props state eff) Unit
didMount a = ask >>= \e -> modify _ { componentDidMount = flip (dispatch e) a }

willUnmount :: forall eval props state action eff. Dispatchable eval (ReactThis props state) action (Eff (props :: R.ReactProps, state :: R.ReactState ReadOnly, refs :: ReactRefs ReadOnly | eff) Unit)
  => action -> RWS eval Unit (ReactSpec props state eff) Unit
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

newtype ReactState s = ReactState s
newtype ReactProps p = ReactProps p
newtype ReactChildren = ReactChildren (Array ReactElement)

instance stateFromContext :: TypeEquals state state2 => FromContext eval (ReactThis props state) (ReactState state2) Eff (state::R.ReactState (read::Read)|eff) where
  fromContext _ = map (ReactState <<< to) <<< readState

instance propsFromContext :: TypeEquals props props2 => FromContext eval (ReactThis props state) (ReactProps props2) Eff (props::R.ReactProps|eff) where
  fromContext _ = map (ReactProps <<< to) <<< R.getProps

instance childrenFromContext :: FromContext eval (ReactThis props state) ReactChildren Eff (props::R.ReactProps|eff) where
  fromContext _ = map ReactChildren <<< getChildren
