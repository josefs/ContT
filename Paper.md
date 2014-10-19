# ContT
# Josef Svenningsson
#

# Intro

# Overview of the technique

## The mother of all Monads

The continuation monad has been called the mother of all monads (sigfpe).

~~~ {.haskell}
newtype Eff r a = Eff { runEff :: (a -> r) -> r }

instance Monad (Eff r) where
  return a = Eff (\k -> k a)
  Cont f >>= m = Eff (\k -> f (\a -> runCont (m a) k))

instance Applicative (Eff r) where
  pure = return
  (<*>) = ap

instance Functor (Eff r) where
  fmap f c = c >>= \a -> return (f a)
~~~

## Adding effects

TODO: Maybe this text should go earlier in the document since it summarises the
essential contribution of the paper.

The crucial idea in this paper it to transform the return type of the
continuation monad. As a first concrete example we will see how to do
this to achieve an effect similar to the reader monad.

~~~ {.haskell}
type Reader e r = e -> r

ask :: Eff (Reader e r) e
ask = Eff $ \k r -> k r r

local :: e -> Eff (Reader e r) a -> Eff (Reader e r) a
local e (Eff f) = Eff $ \k e' -> f (\a _ -> k a e') e
~~~

## Other effects

State, writer, error.

What about continuation effect?

# More type safety

## A first attempt

~~~ {.haskell}
newtype Reader e r = Reader (e -> r)
~~~

## A better solution

~~~{.haskell}
newtype ReaderT e = ReaderT { getR :: e }
type Reader e r = ReaderT e -> r
~~~

## Overloading operations

# Measurements

Compare with mtl, algebraic effects.

Measure performance of composed hierarchy vs hand-rolled monad. For
instance Parsec.

# Discussion

## Future work

How complete is this model of effects?

Can we achieve the composability of algebraic effects while still
retaining performance?
