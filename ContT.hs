module ContT where

import Control.Applicative
import Control.Monad

import Data.Monoid

-- Continuation monad

newtype Cont r a = Cont { runCont :: ((a -> r) -> r) }

instance Functor (Cont r) where
  fmap f c = c >>= \a -> return (f a)

instance Monad (Cont r) where
  return a = Cont (\k -> k a)
  Cont f >>= m = Cont (\k -> f (\a -> runCont (m a) k))

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

-- Transformers

type Write w r = w -> r
type State s r = s -> r
type Reader e r = e -> r
type Error e r = (e -> r) -> r

runWriterCT :: Monoid w => Cont (Write w r) a -> Cont r (a,w)
runWriterCT (Cont f) = Cont $ \k -> f (\a w -> k (a,w)) mempty

tellCT :: Monoid w => w -> Cont (Write w r) ()
tellCT w = Cont $ \k w' -> k () (w' <> w)

getCT :: Cont (State s r) s
getCT = Cont $ \k s -> k s s

setCT :: s -> Cont (State s r) s
setCT s = Cont $ \k s' -> k s' s

runStateCT :: Cont (State s r) a -> s -> Cont r (a,s)
runStateCT (Cont f) s = Cont $ \k -> f (\a s -> k (a,s)) s

throwCT :: e -> Cont (Error e r) a
throwCT e = Cont $ \k k' -> k' e

catchCT :: Cont (Error e r) a -> (e -> Cont (Error e r) a) -> Cont (Error e r) a
catchCT (Cont f) h = Cont $ \k k' -> f k (\e -> runCont (h e) k k')

-- It's really sad that I have to go back and forth between errors.
-- This is not required in mtl, where there is only a function (e -> e')
-- Maybe if I could come up with a simpler error effect I could have a simpler type
withError :: (e -> e') -> (e' -> e) -> Cont (Error e r) a -> Cont (Error e' r) a
withError t d (Cont f) = Cont $ \k k' -> f (\a k'' -> k a (\e -> k'' (d e))) (\e -> k' (t e))

runError :: Cont (Error e r) a -> Cont r (Either e a)
runError (Cont f) = Cont $ \k -> f (\a k' -> k (Right a)) (\e -> k (Left e))

envCT :: Cont (Reader e r) e
envCT = Cont $ \k r -> k r r

local :: e -> Cont (Reader e r) a -> Cont (Reader e r) a
local e (Cont f) = Cont $ \k e' -> f (\a _ -> k a e') e

runReader :: Cont (Reader e r) a -> e -> Cont r a
runReader (Cont f) e = Cont $ \k -> f (\a _ -> k a) e

-- This function can lift any operation through any effect which
-- is of the form (e -> r).
-- The question is, can all effects be written on this form?
liftCT :: Cont r a -> Cont (e -> r) a
liftCT (Cont f) = Cont $ \k e -> f (\a -> k a e)

{-
How do we catch computations which doesn't have error on the top?
It doesn't work in mtl either, which is some consolation at least.

testCT = flip runCont id $ runError $ flip runStateCT 1 $
         catchCT
         (do s <- getCT
             liftCT $ throwCT "foo"
             setCT 7)
         (\e -> setCT 18)
-}

testCT = flip runCont id $ flip runStateCT 1 $ runError $
         catchCT
         (do s <- liftCT getCT
             liftCT $ setCT 7
             throwCT "foo")
         (\e -> liftCT $ setCT 18)

testTell = flip runCont id $ runWriterCT $ tellCT "foo" >> tellCT "bar"

testEnv = flip runCont id $ flip runReader 1 envCT
testLocal = flip runCont id $ flip runReader 1 $
            local 10 envCT

--------------------------------------------------

-- I should be able to overload the operations by
-- using an identity newtype on the parameter

newtype ReaderT e = ReaderT { getR :: e }
type Reader' e r = ReaderT e -> r

newtype StateT s = StateT { getS :: s }
type State' s r = StateT s -> r

