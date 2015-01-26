{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- Here I use a higher order return type for my continuation monad.
   It seems be required for implementing LogicT.

The drawback of this representation is that it requires the arguments
in the return type to be newtypes, because they are higher kinded.
Type synonyms may not be partially applied. It remains to be seen
if this is a problem and will cause things to go slower.
It does mean that we lose the generic lift function though.
-}

module ContTT where

import Control.Applicative
import Control.Monad

import Control.Monad.ST

data Cont m a = Cont { runCont :: forall b. ((a -> m b) -> m b) }

instance Functor (Cont m) where
  fmap f (Cont m) = Cont $ \k -> m (k . f)

instance Applicative (Cont m) where
  pure a = Cont $ \k -> k a
  Cont f <*> Cont a = Cont $ \k -> f (\fun -> a (\arg -> k (fun arg)))

instance Monad (Cont m) where
  return a = Cont $ \k -> k a
  Cont m >>= f = Cont $ \k -> m (\a -> runCont (f a) k)

{- Deriving LogicT based on the LogicT in the paper

Cont (LogicT r') a = (forall b. (a -> m b -> m b) -> m b -> m b)
(forall b. (a -> LogicT r' b) -> LogicT r' b) = (forall b. (a -> m b -> m b) -> m b -> m b
LogicT r' b = r' b -> r' b
-}

newtype LogicT r a = LogicT { unLogic :: Cont r a -> Cont r a }

instance MonadPlus (Cont (LogicT r)) where
  mzero = Cont $ \sk -> LogicT $ \fk -> fk
  Cont m `mplus` Cont n = Cont $ \sk -> LogicT $ \fk ->
   unLogic (m sk) (unLogic (n sk) fk)

instance Alternative (Cont (LogicT r)) where
  empty = mzero
  (<|>) = mplus

reflect :: MonadPlus m => Maybe (a, m a) -> m a
reflect r = case r of
              Nothing -> mzero
              Just (a, tmr) -> return a `mplus` tmr

liftT :: Cont r a -> Cont (LogicT r) a
liftT (Cont m) = Cont $ \sk -> LogicT $ \fk ->
  Cont $ \k -> (m (\a -> runCont (unLogic (sk a) fk) k))

msplit :: Cont (LogicT r) a ->
          Cont (LogicT r) (Maybe (a,Cont (LogicT r) a))
msplit (Cont m) = liftT (unLogic (m ssk) (return Nothing))
  where ssk a = LogicT $ \fk -> return (Just (a,liftT fk >>= reflect))

rr tm = msplit tm >>= reflect

-- Reader

newtype Reader e m r = Reader { unReader :: e -> m r }

liftReader :: Cont m a -> Cont (Reader e m) a
liftReader (Cont f) = Cont $ \k -> Reader $ \e -> f (\a -> unReader (k a) e)

envReader :: Cont (Reader e m) e
envReader = Cont $ \k -> Reader $ \e -> unReader (k e) e

localReader :: e -> Cont (Reader e m) a -> Cont (Reader e m) a
localReader e (Cont f) = Cont $ \k -> Reader $ \e' -> unReader (f (\a -> Reader (\e'' -> unReader (k a) e'))) e

runReader :: e -> Cont (Reader e m) a -> Cont m a
runReader e (Cont f) = Cont $ \k -> unReader (f (\a -> Reader $ \e -> k a)) e

class ReaderM m where
  type Env m
  env :: Cont m (Env m)
  local :: (Env m) -> Cont m a -> Cont m a

instance ReaderM (Reader e m) where
  type Env (Reader e m) = e
  env = envReader
  local = localReader

instance ReaderM m => ReaderM (State s m) where
  type Env (State s m) = Env m
  env = liftState env
  local e (Cont f) = Cont $ \k -> State $ \s -> 
    runCont (local e (Cont (\k' -> unState (f (\a -> State $ \s' -> k' a)) s))) (\a -> unState (k a) s)

-- State

newtype State s m r = State { unState :: s -> m r }

liftState :: Cont m a -> Cont (State s m) a
liftState (Cont f) = Cont $ \k -> State $ \e -> f (\a -> unState (k a) e)

getState :: Cont (State s m) s
getState = Cont $ \k -> State $ \s -> unState (k s) s

setState :: s -> Cont (State s m) s
setState s = Cont $ \k -> State $ \s_old -> unState (k s_old) s

runState :: s -> Cont (State s m) a -> Cont m (s,a)
runState s (Cont f) = Cont $ \k -> unState (f (\a -> State $ \s' -> k (s',a))) s

-- ST

conv :: (forall s. Cont (ST s) a) -> (forall s r. (a -> ST s r) -> ST s r)
conv (Cont f) = f

runSTT :: (forall s. Cont (ST s) a) -> a
runSTT c = runST (conv c return)
