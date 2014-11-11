{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{- Here I use a higher order return type for my continuation monad.
   It seems be required for implementing LogicT.

The drawback of this representation is that it requires the arguments
in the return type to be newtypes, because they are higher kinded.
Type synonyms may not be partially applied. It remains to be seen
if this is a problem and will cause things to go slower.
-}

module ContTT where

import Control.Applicative
import Control.Monad

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

