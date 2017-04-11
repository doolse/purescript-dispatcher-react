module Dispatcher (
  dispatch
, class Dispatchable
, effEval
, fromContext
, class FromContext
, Context(..)
, DispatchAff(..)
, DispatchEff(..)
, DispatchEffFn(..)
, DispatchEffFn2(..)
, DispatchEffFn3(..)
) where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3)
import Data.Maybe (Maybe, maybe)
import Type.Equality (class TypeEquals, to)

effEval ::  forall a context eff. (a -> ReaderT context (Eff eff) Unit) -> a -> ReaderT context (Eff eff) Unit
effEval = id

class Dispatchable eval context action result | eval -> context where
  dispatch :: eval -> context -> action -> result

-- | An instance of `Dispatchable` for optionally dispatching an action using `Maybe`
instance funcMaybeDispatch :: (Applicative m,  Dispatchable (action -> dispatchable) context dispatchable (m Unit))
  => Dispatchable (action -> dispatchable) context (Maybe action) (m Unit) where
  dispatch f c ma = maybe (pure unit) (\a -> dispatch f c (f a)) ma

instance funcDispatch :: Dispatchable (action -> next) context next m => Dispatchable (action -> next) context action m where
  dispatch f c a = dispatch f c (f a)

instance readerTTest :: Dispatchable eval context (m a) n => Dispatchable eval context (ReaderT context m a) n where
  dispatch e c a = dispatch e c $ runReaderT a c

instance effDispacher :: Dispatchable eval context (Eff eff a) (Eff eff2 a) where
  dispatch _ _ = unsafeCoerceEff

instance affDispacher :: Dispatchable eval context (Aff eff a) (Aff eff2 a) where
  dispatch _ _ = unsafeCoerceAff

instance aff2EffDispacher :: Dispatchable eval context (Aff eff a) (Eff eff2 Unit) where
  dispatch e c = void <<< unsafeCoerceEff <<< launchAff

class FromContext eval context a m (eff :: # Effect) | eval context a m -> eff where
  fromContext :: eval -> context -> m eff a

newtype DispatchEff action = DispatchEff (forall event eff. (event -> action) -> event -> Eff eff Unit)

newtype DispatchAff action = DispatchAff (forall event eff. (event -> action) -> event -> Aff eff Unit)

newtype DispatchEffFn action = DispatchEffFn (forall event eff. (event -> action) -> EffFn1 eff event Unit)

newtype DispatchEffFn2 action = DispatchEffFn2 (forall event ev2 eff. (event -> ev2 -> action) -> EffFn2 eff event ev2 Unit)

newtype DispatchEffFn3 action = DispatchEffFn3 (forall event ev2 ev3 eff. (event -> ev2 -> ev3 -> action) -> EffFn3 eff event ev2 ev3 Unit)

instance dispatchAffD :: (Applicative (m eff2), Dispatchable eval context action (Aff eff Unit))
  => FromContext eval context (DispatchAff action) m eff2 where
  fromContext eval c = pure $ DispatchAff (\handle ev -> void $ unsafeCoerceAff $ (dispatch eval c (handle ev)) :: Aff eff Unit)

instance dispatchEffD :: (Applicative (m eff2), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEff action) m eff2 where
  fromContext eval c = pure $ DispatchEff (\handle ev -> void $ unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFnD :: (Applicative (m eff2), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn (\handle -> mkEffFn1 \ev -> void $ unsafeCoerceEff $ (dispatch eval c (handle ev)) :: Eff eff Unit)

instance dispatchEffFn2D :: (Applicative (m eff2), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn2 action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn2 (\handle -> mkEffFn2 \ev ev2 -> void $ unsafeCoerceEff $ (dispatch eval c (handle ev ev2)) :: Eff eff Unit)

instance dispatchEffFn3D :: (Applicative (m eff2), Dispatchable eval context action (Eff eff Unit))
  => FromContext eval context (DispatchEffFn3 action) m eff2 where
  fromContext eval c = pure $ DispatchEffFn3 (\handle -> mkEffFn3 \ev ev2 ev3 -> void $ unsafeCoerceEff $ (dispatch eval c (handle ev ev2 ev3)) :: Eff eff Unit)

newtype Context a = Context a

instance contextFromContext :: (TypeEquals context context2, Applicative (m eff)) => FromContext eval context (Context context2) m eff where
  fromContext _ c = pure (Context $ to c)
