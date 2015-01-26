{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
module ContT where

import Control.Applicative
import Control.Monad

import Data.Monoid

import Control.Monad.ST

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

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

-- Transformers

type Write w r = w -> r
type State s r = s -> r
type Reader e r = e -> r
type Error e r = (e -> r) -> r
type Kont a r = (a -> r) -> r

type Logic r = r -> r

runWriterCT :: Monoid w => Cont (Write w r) a -> Cont r (a,w)
runWriterCT (Cont f) = Cont $ \k -> f (\a w -> k (a,w)) mempty

tellCT :: Monoid w => w -> Cont (Write w r) ()
tellCT w = Cont $ \k w' -> k () (w' <> w)

-- Less useful in this library as it uses a monoid to concatenate
-- results. If the programmer where to just replace the result then
-- this function would be much more powerful.
collectCT :: Cont (Write w r) w
collectCT = Cont $ \k w -> k w w

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

localCT :: e -> Cont (Reader e r) a -> Cont (Reader e r) a
localCT e (Cont f) = Cont $ \k e' -> f (\a _ -> k a e') e

runReaderCT :: Cont (Reader e r) a -> e -> Cont r a
runReaderCT (Cont f) e = Cont $ \k -> f (\a _ -> k a) e

callCCk :: ((a -> Cont (Kont b r) b) -> Cont (Kont a r) a) -> Cont (Kont a r) a
callCCk = undefined
--callCCk f = Cont $ \k k' -> f (\a -> k' a) k'
{-
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

-}
-- This function can lift any operation through any effect which
-- is of the form (e -> r).
-- The question is, can all effects be written on this form?
liftCT :: Cont r a -> Cont (e -> r) a
liftCT (Cont f) = Cont $ \k e -> f (\a -> k a e)

-- Logic

observe :: Cont (Logic r) a -> Cont r a
observe (Cont f) = Cont $ \k -> f (const . k) (error "No answer.")

--observeAll :: Cont (Logic r) a -> Cont r [a]
--observeAll (Cont f) = Cont $ \k -> f (\a r -> k [a]) (k [])

empty :: Cont (Logic r) a
empty = Cont $ \_ fk -> fk

(<|>) :: Cont (Logic r) a -> Cont (Logic r) a -> Cont (Logic r) a
Cont f1 <|> Cont f2 = Cont $ \sk fk -> f1 sk (f2 sk fk)

msplit :: Cont (Logic r) a -> Cont (Logic r) (Maybe (a,Cont (Logic r) a))
msplit (Cont f) = Cont $ \sk fk ->
                    f (\a fk' -> sk (Just (a,Cont $ \_ _ -> f (\_ r -> r) fk')) fk)
                      (sk Nothing fk)
{-

Prove msplit laws

msplit mzero = return Nothing
msplit (return a ‘mplus‘ m) = return (Just (a, m))

1)
msplit mzero =
msplit (Cont (\_ fk -> fk)) =
Cont $ \sk fk -> (\_ fk' -> fk') (\a fk -> ...) (sk Nothing fk) =
Cont $ \sk fk -> sk Nothing fk

return Nothing =
Cont $ \sk -> sk Nothing

2) -- fails pretty badly so far
msplit (return a <|> m) = { return }
msplit ((Cont (\sk fk -> sk a fk)) <|> (Cont f)) = { (<|>) }
msplit (Cont $ \skp fkp -> (\sk fk -> sk a fk) skp (f skp fkp)) =
msplit (Cont $ \skp fkp -> skp a (f skp fkp)) = { msplit }
Cont $ \sk fk -> (\skp fkp -> skp a (f skp fkp))
                    (\b fk' -> sk (Just (b,Cont $ \_ _ -> fk')) fk)
                    (sk Nothing fk) =
Cont $ \sk fk -> (\b fk' -> sk (Just (b,Cont $ \_ _ -> fk')) fk) a
  (f (\b fk' -> sk (Just (b,Cont $ \_ _ -> fk')) fk) (sk Nothing fk)) =
Cont $ \sk fk -> sk (Just (a,Cont $ \_ _ -> fk')) fk
  where fk' = f (\b fk' -> sk (Just (b,Cont $ \_ _ -> fk')) fk) (sk Nothing fk)

return (Just (a,m)) = Cont $ \sk -> sk (Just (a,m))

-}

reflect :: Maybe (a,Cont (Logic r) a) -> Cont (Logic r) a
reflect Nothing = ContT.empty
reflect (Just (a,m)) = return a ContT.<|> m

{-
newtype LogicT r = LogicT{ unLogicT :: forall b. Cont r b -> Cont r b }

This type can never work because it gets the forall quantifier in the
wrong place. The continuation type becomes like this:

(a -> (forall b . Cont r b -> Cont r b)) -> (forall b. Cont r b -> Cont r b)

But we wanted :

forall b . (a -> (Cont r b -> Cont r b)) -> (Cont r b -> Cont r b)


failL :: LogicT r
failL = LogicT (\fk -> fk)

composeL :: (t -> LogicT r) -> (t -> LogicT r) -> t -> LogicT r
composeL f1 f2 sk = LogicT (\fk -> unLogicT (f1 sk) (unLogicT (f2 sk) fk))

emptyT :: Cont (LogicT r) a
emptyT = Cont $ \_ -> failL

(<+>) :: Cont (LogicT r) a -> Cont (LogicT r) a -> Cont (LogicT r) a
Cont f1 <+> Cont f2 = Cont (composeL f1 f2)

liftL :: Cont r a -> Cont (LogicT r) a
liftL (Cont f) = Cont $ \sk -> LogicT (\fk -> Cont (\k -> f (\a -> runCont (unLogicT (sk a) fk) k)))

msplitT :: Cont (LogicT r) a -> Cont (LogicT r) (Maybe (a,Cont (LogicT r) a))
msplitT (Cont m) = liftL $ unLogicT (m ssk) (return Nothing)
  where ssk a = LogicT (\fk -> return (Just a, liftL fk >>= reflectL))

reflectL :: Maybe (a,Cont (LogicT r) a) -> Cont (LogicT r) a
reflectL Nothing = emptyT
reflectL (Just (a,m)) = return a <+> m
-}

type LogicT r' r = Cont r r' -> Cont r r'

emptyT :: Cont (LogicT r' r) a
emptyT = Cont $ \_ fk -> fk

(<+>) :: Cont (LogicT r' r) a -> Cont (LogicT r' r) a -> Cont (LogicT r' r) a
Cont m <+> Cont n = Cont $ \sk fk -> m sk (n sk fk)

reflectT :: Maybe (a, Cont (LogicT r' r) a) -> Cont (LogicT r' r) a
reflectT Nothing = emptyT
reflectT (Just (a,m)) = return a <+> m

liftL :: Cont r a -> Cont (LogicT r r) a
liftL (Cont f) = Cont $ \sk fk -> return (f (\a -> runCont (sk a fk) id))

msplitL :: Cont (LogicT (Maybe (a,Cont (LogicT r r) a)) r) a ->
           Cont (LogicT r r) (Maybe (a,Cont (LogicT r r) a))
msplitL (Cont m) = liftL $ m ssk (return Nothing)
  where ssk a fk = return $ Just (a, (liftL fk >>= reflect))

{-
    msplit m = lift $ unLogicT m ssk (return Nothing)
     where
     ssk a fk = return $ Just (a, (lift fk >>= reflect))
-}

observeT :: Cont (LogicT r r) a -> Cont r a
observeT (Cont m) = Cont $ \k -> runCont (m (\a fk -> return (k a)) (fail "No answer.")) id

observeAllT :: Cont (LogicT [a] r) a -> Cont r [a]
observeAllT (Cont m) = Cont $ \k ->
  runCont (m (\a fk -> fmap (a:) fk) (return [])) k

{- Doesn't type check. msplit is not polymorphic enough.
interleave sg1 sg2 = do
  r <- msplitL sg1
  case r of
    Nothing -> sg2
    Just (a,sg1') ->
      return a <+> interleave sg2 sg1'
-}
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

testCT = flip runCont id $ flip runStateCT 1 $ runError comp

comp = catchCT
         (do s <- liftCT getCT
             liftCT $ setCT (7 :: Integer)
             throwCT "foo")
         (\e -> return ())

testTell = flip runCont id $ runWriterCT $ tellCT "foo" >> tellCT "bar"

testEnv = flip runCont id $ flip runReaderCT 1 envCT
testLocal = flip runCont id $ flip runReaderCT 1 $
            localCT 10 envCT

--------------------------------------------------

-- Example of type unsafety:

unsafe = flip runCont id $ runWriterCT $
         do tellCT "foo"
            setCT "bar"
            envCT

-- I should be able to overload the operations by
-- using an identity newtype on the parameter

newtype ReaderT e = ReaderT { getR :: e }
type Reader' e r = ReaderT e -> r

newtype StateT s = StateT { getS :: s }
type State' s r = StateT s -> r

newtype WriterT w = WriterT { getW :: w }
type Writer' w r = WriterT w -> r

newtype ErrorT e = ErrorT { getE :: e }
type Error' e r = (ErrorT e -> r) -> r

newtype ListT r = ListT { getL :: r }
type ListT' r = ListT r -> r

tell :: Monoid w => w -> Cont (Writer' w r) ()
tell w = Cont $ \k w' -> k () (WriterT $ getW w' <> w)

collect :: Cont (Writer' w r) w
collect = Cont $ \k w -> k (getW w) w

runWriter :: Monoid w => Cont (Writer' w r) a -> Cont r (a,w)
runWriter (Cont f) = Cont $ \k -> f (\a w -> k (a,getW w)) (WriterT mempty)

get :: Cont (State' s r) s
get = Cont $ \k s -> k (getS s) s

set :: s -> Cont (State' s r) s
set s = Cont $ \k s' -> k (getS s') (StateT s)

runState :: Cont (State' s r) a -> s -> Cont r (a,s)
runState (Cont f) s = Cont $ \k -> f (\a s -> k (a,getS s)) (StateT s)

env :: Cont (Reader' e r) e
env = Cont $ \k r -> k (getR r) r

local :: e -> Cont (Reader' e r) a -> Cont (Reader' e r) a
local e (Cont f) = Cont $ \k e' -> f (\a _ -> k a e') (ReaderT e)

runReader :: Cont (Reader' e r) a -> e -> Cont r a
runReader (Cont f) e = Cont $ \k -> f (\a _ -> k a) (ReaderT e)

throw :: e -> Cont (Error' e r) a
throw e = Cont $ \k k' -> k' (ErrorT e)

catch :: Cont (Error' e r) a -> (e -> Cont (Error' e r) a) -> Cont (Error' e r) a
catch (Cont f) h = Cont $ \k k' -> f k (\e -> runCont (h (getE e)) k k')

-- It's really sad that I have to go back and forth between errors.
-- This is not required in mtl, where there is only a function (e -> e')
-- Maybe if I could come up with a simpler error effect I could have a simpler type
withError' :: (e -> e') -> (e' -> e) -> Cont (Error' e r) a -> Cont (Error' e' r) a
withError' t d (Cont f) = Cont $ \k k' -> f (\a k'' -> k a (\e -> k'' (ErrorT (d (getE e))))) (\e -> k' (ErrorT (t (getE e))))

runError' :: Cont (Error' e r) a -> Cont r (Either e a)
runError' (Cont f) = Cont $ \k -> f (\a k' -> k (Right a)) (\e -> k (Left (getE e)))

combine :: Cont (ListT' r) a -> Cont (ListT' r) a -> Cont (ListT' r) a
combine (Cont f) (Cont g) = Cont $ \k r -> f k (ListT (g k r))

emptyListT :: Cont (ListT' r) a
emptyListT = Cont $ \k r -> getL r

{- Seems impossible to write!
runListT' :: Cont (ListT' r) a -> Cont r [a]
runListT' (Cont f) = Cont $ \k -> f (\a r -> k [a]) (ListT (k []))

observeAll :: Cont (ListT' r) a -> Cont (ListT' r) [a]
observeAll (Cont f) = Cont $ \k r -> f (\a r -> k [a] r) (ListT (k [] r))
-}

runListT' :: (forall r. Cont (ListT' r) a) -> [a]
runListT' (Cont f) = flip runCont id $ Cont $ \k -> f (\a r -> a : getL r) (ListT (k []))

lift :: Cont r a -> Cont (e -> r) a
lift (Cont f) = Cont $ \k e -> f (\a -> k a e)

{- This now doesn't type check. Success!
unsafe' = flip runCont id $ runWriter $
          do tell "foo"
             set "bar"
             env
-}

-- IO

class IOM r where
  liftIO :: IO a -> Cont r a

instance IOM (IO r) where
  liftIO m = Cont $ \k -> m >>= k

instance IOM r => IOM (e -> r) where
  liftIO m = lift (liftIO m)

runIO :: (forall r. Cont (IO r) a) -> IO a
runIO (Cont f) = f return

class STM r where
  type STP r
  liftST :: ST (STP r) a -> Cont r a

instance STM (ST s r) where
  type STP (ST s r) = s
  liftST m = Cont $ \k -> m >>= k

instance STM r => STM (e -> r) where
  type STP (e -> r) = STP r
  liftST m = lift (liftST m)

{- Polymorphism problems
runSTT :: (forall s r. Cont (ST s r) a) -> a
runSTT (Cont f) = runST (f return)
-}

-- Doing the overloading like this means we have the quadratic
-- behaviour that's so unfortunate for all monad transformer libraries
class ReaderM r where
  type Env r
  envC :: Cont r (Env r)
  localC :: (Env r) -> Cont r a -> Cont r a

instance ReaderM (ReaderT e -> r) where
  type Env (ReaderT e -> r) = e
  envC = env
  localC = local

instance ReaderM r => ReaderM (StateT s -> r) where
  type Env (StateT s -> r) = Env r
  envC = lift envC
  localC e (Cont f) = Cont $ \k s -> runCont (localC e (Cont (\k' -> f (\a s -> k' a) s))) (\a -> k a s)

instance ReaderM r => ReaderM (ErrorT e -> r) where
  type Env (ErrorT e -> r) = Env r
  envC = lift envC
{-
local :: e -> Cont (Reader' e r) a -> Cont (Reader e r) a
local e (Cont f) = Cont $ \k e' -> f (\a _ -> k a e') (ReaderT e)
-}
{-
type family Env e r where
  Env (ReaderT e) r = e
  Env a (e -> r) = Env e r

class ReaderM e r where
  envC :: Cont (e -> r) (Env e r)

instance ReaderM (ReaderT e) r where
  envC = env

instance (ReaderM e r,e :/~ (ReaderT q)) => ReaderM e (a -> r) where
  envC = lift envC

envChelp :: (ReaderM a r, e :/~ (ReaderT q)) => Cont (e -> a -> r) (Env a r)
envChelp = lift envC

The stuff below is from:
https://stackoverflow.com/questions/6939043/is-it-possible-to-place-inequality-constraints-on-haskell-type-variables

data Yes = Yes deriving (Show)
data No = No deriving (Show)

class (TypeEq x y No) => (:/~) x y
instance (TypeEq x y No) => (:/~) x y

class (TypeEq' () x y b) => TypeEq x y b where
    typeEq :: x -> y -> b
    maybeCast :: x -> Maybe y

instance (TypeEq' () x y b) => TypeEq x y b where
    typeEq x y = typeEq' () x y
    maybeCast x = maybeCast' () x

class TypeEq' q x y b | q x y -> b where
    typeEq' :: q -> x -> y -> b
    maybeCast' :: q -> x -> Maybe y

instance (b ~ Yes) => TypeEq' () x x b where
    typeEq' () _ _ = Yes
    maybeCast' _ x = Just x

instance (b ~ No) => TypeEq' q x y b where
    typeEq' _ _ _ = No
    maybeCast' _ _ = Nothing

const' :: (a :/~ b) => a -> b -> a
const' x _ = x
-}
{- This is a bit of type hackery to achieve type inequality
   Unfortunately it doesn't work.
   I added `TypeNeq a (ReaderT q)` to the context for the
   ReaderM e (a -> r) instance but to no avail.

data True
data False

type family TypeEqF a b where
  TypeEqF a a = True
  TypeEqF a b = False

type TypeNeq a b = TypeEqF a b ~ False
-}
{- Apparently there is no such thing as closed data families
data family TypeEqF a b where
  TypeEqF a a = True
  TypeEqF a b = False
-}

-- Testing the laziness of the state monad
-- It is clearly strict.

foo = flip runCont id $ flip runState 10 $ do
    xs <- take 100 `fmap` sequence (repeat get)
    set (last xs)


-- Experiments with the continuations

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f (Cont h) = Cont $ \k -> f (h (\a -> f (k a)))

mapCont2 :: (r -> r') -> (r' -> r) -> Cont r a -> Cont r' a
mapCont2 f g (Cont h) = Cont $ \k -> f (h (\a -> g (k a)))

{- This is never going to work because 'r' occurs both
   positively and negatively in Cont
bindCont :: Cont r a -> (r -> Cont s a) -> Cont s a
bindCont (Cont f) g = Cont $ \k -> 
-}

{- Function inspired by Mauro's Monatron library. But I don't
   know how to give it a sensible type in my framework.
tmap :: Cont (e -> r) a -> Cont (e' -> r) a
-}

