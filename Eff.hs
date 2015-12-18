{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Eff where

import Control.Applicative
import Control.Monad

import Control.Monad.ST
import Data.STRef (STRef)
import qualified Data.STRef as S

import Data.Monoid

data Eff m a = Eff { runEff :: forall b. ((a -> m b) -> m b) }

instance Functor (Eff m) where
  fmap f (Eff m) = Eff $ \k -> m (k . f)

instance Applicative (Eff m) where
  pure a = Eff $ \k -> k a
  Eff f <*> Eff a = Eff $ \k -> f (\fun -> a (\arg -> k (fun arg)))

instance Monad (Eff m) where
  return a = Eff $ \k -> k a
  Eff m >>= f = Eff $ \k -> m (\a -> runEff (f a) k)

-- LogicT

newtype LogicT r a = LogicT { unLogic :: Eff r a -> Eff r a }

instance MonadPlus (Eff (LogicT r)) where
  mzero = Eff $ \sk -> LogicT $ \fk -> fk
  Eff m `mplus` Eff n = Eff $ \sk -> LogicT $ \fk ->
   unLogic (m sk) (unLogic (n sk) fk)

instance Alternative (Eff (LogicT r)) where
  empty = mzero
  (<|>) = mplus

reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect r = case r of
              Nothing -> mzero
              Just (a, tmr) -> return a `mplus` tmr

liftT :: Eff r a -> Eff (LogicT r) a
liftT (Eff m) = Eff $ \sk -> LogicT $ \fk ->
  Eff $ \k -> (m (\a -> runEff (unLogic (sk a) fk) k))

msplit :: Eff (LogicT r) a ->
          Eff (LogicT r) (Maybe (a,Eff (LogicT r) a))
msplit (Eff m) = liftT (unLogic (m ssk) (return Nothing))
  where ssk a = LogicT $ \fk -> return (Just (a,liftT fk >>= reflect))

rr tm = msplit tm >>= reflect

-- Reader

newtype Reader e m r = Reader { unReader :: e -> m r }

liftReader :: Eff m a -> Eff (Reader e m) a
liftReader (Eff f) = Eff $ \k -> Reader $ \e -> f (\a -> unReader (k a) e)

envReader :: Eff (Reader e m) e
envReader = Eff $ \k -> Reader $ \e -> unReader (k e) e

localReader :: e -> Eff (Reader e m) a -> Eff (Reader e m) a
localReader e (Eff f) = Eff $ \k -> Reader $ \e' -> unReader (f (\a -> Reader (\e'' -> unReader (k a) e'))) e

runReader :: e -> Eff (Reader e m) a -> Eff m a
runReader e (Eff f) = Eff $ \k -> unReader (f (\a -> Reader $ \e -> k a)) e

class ReaderM m where
  type Env m
  env :: Eff m (Env m)
  local :: (Env m) -> Eff m a -> Eff m a

instance ReaderM (Reader e m) where
  type Env (Reader e m) = e
  env = envReader
  local = localReader

instance ReaderM m => ReaderM (State s m) where
  type Env (State s m) = Env m
  env = liftState env
  local e (Eff f) = Eff $ \k -> State $ \s -> 
    runEff (local e (Eff (\k' -> unState (f (\a -> State $ \s' -> k' a)) s))) (\a -> unState (k a) s)

-- State

newtype State s m r = State { unState :: s -> m r }

liftState :: Eff m a -> Eff (State s m) a
liftState (Eff f) = Eff $ \k -> State $ \e -> f (\a -> unState (k a) e)

getState :: Eff (State s m) s
getState = Eff $ \k -> State $ \s -> unState (k s) s

setState :: s -> Eff (State s m) s
setState s = Eff $ \k -> State $ \s_old -> unState (k s_old) s

runState :: s -> Eff (State s m) a -> Eff m (s,a)
runState s (Eff f) = Eff $ \k -> unState (f (\a -> State $ \s' -> k (s',a))) s

-- Error

newtype Error e m r = Error { unError :: (e -> m r) -> m r }

throw :: e -> Eff (Error e m) a
throw e = Eff $ \k -> Error $ \k' -> k' e

catch :: Eff (Error e m) a -> (e -> Eff (Error e m) a) -> Eff (Error e m) a
catch (Eff f) h = Eff $ \k -> Error $ \k' -> unError (f k) (\e -> unError (runEff (h e) k) k')

-- It's really sad that I have to go back and forth between errors.
-- This is not required in mtl, where there is only a function (e -> e')
-- Maybe if I could come up with a simpler error effect I could have a simpler type
withError :: (e -> e') -> (e' -> e) -> Eff (Error e m) a -> Eff (Error e' m) a
withError t d (Eff f) = Eff $ \k -> Error $ \k' -> unError (f (\a -> Error $ \k'' -> unError (k a) (\e -> k'' (d e)))) (\e -> k' (t e))

runError :: Eff (Error e m) a -> Eff m (Either e a)
runError (Eff f) = Eff $ \k -> unError (f (\a -> Error $ \k' -> k (Right a))) (\e -> k (Left e))

instance Monoid e => MonadPlus (Eff (Error e m)) where
  mzero = throw mempty
  mplus m1 m2 = catch m1 (\_ -> m2)

instance Monoid e => Alternative (Eff (Error e m)) where
  empty = mzero
  (<|>) = mplus

-- ST

-- ST doesn't have a lift function in the same sense as the other
-- effect transformers above

doST :: ST s a -> Eff (ST s) a
doST m = Eff $ \k -> m >>= k

conv :: (forall s. Eff (ST s) a) -> (forall s r. (a -> ST s r) -> ST s r)
conv (Eff f) = f

runSTT :: (forall s. Eff (ST s) a) -> a
runSTT c = runST (conv c return)

newSTRef :: a -> Eff (ST s) (STRef s a)
newSTRef a = doST (S.newSTRef a)

readSTRef :: STRef s a -> Eff (ST s) a
readSTRef ref = doST (S.readSTRef ref)

writeSTRef :: STRef s a -> a -> Eff (ST s) ()
writeSTRef ref a = doST (S.writeSTRef ref a)

modifySTRef :: STRef s a -> (a -> a) -> Eff (ST s) ()
modifySTRef ref f = doST (S.modifySTRef ref f)
modifySTRef' :: STRef s a -> (a -> a) -> Eff (ST s) ()
modifySTRef' ref f = doST (S.modifySTRef' ref f)

-- IO

liftIO :: IO a -> Eff IO a
liftIO m = Eff $ \k -> m >>= k

runIO :: Eff IO a -> IO a
runIO (Eff m) = m return
