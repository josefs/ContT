---
documentclass: 'llncs'
title: 'Composing Effects by Altering the Answer Type'
author: 'Josef Svenningsson'
institute: 'Chalmers University of Technology'
abstract:

  This paper present a new approach to composing effects based on the
  continuation monad. New effects are added by adding structure to the
  answer type. This approach is similar to monad transformers but has
  advantages of requiring less code, guaranteed to satisfy the monad
  laws and normalize computations automatically. Standard examples of
  monad transformers are shown to fit into our framework.

---

# Introduction

Monads are an established technique for programming with effects and
keeping them under control. Unfortunately monads don't compose, which
is the reason for monad transformers [@liang1995monad] and algebraic
effects [@kiselyov2013extensible] which are methods of composing
effects to yield new monads.

In this paper we introduce a new approach to composing computational
effects based on continuations. In our new approach, all computations
happen in the continuation monad. New computational effects are added
by adding structure to the answer type of the continuation monad.

Compared to traditional monad transformer libraries, this new approach
has the advantage that monadic computations are automatically
normalized. It is a well-known trick to use the continuation monad (or
the co-density monad) to speed up monadic computations because they
ensure that any sequence of binds are associated to the right. Having
binds associated to the left can lead to quadratic complexity for some
monads.

Another advantage is that of our approach is that it guarantees to
satisfy the monad laws. An infamous example in the monad transformer
world is the ListT monad transformer which uses a straightforward
implementation but fails to satisfy the monad laws in general
[@ListT]. Because of the way we base our effects on the continuation
monad the monad laws hold for all effects that we may add.

Finally, our approach requires less to implement effects compared to
monad transformers. New effects do not need any corresponding monad-
or monad transformer instance.

We present a new Haskell library for composing effects. The library
contains effects which corresponds to the usual monad
transformers typically found in monad transformer libraries.

# Overview of the technique

In this section we will present the central idea of our approach and
show how to embed standard effects.

## The continuation monad

As mentioned in the introduction, our library for is based on the
continuation monad. We use it to form the base monad in the library.
Following [@kiselyov2013extensible], the based monad is called `Eff`,
short for effect.

~~~ {.haskell}
newtype Eff r a = Eff { runEff :: (a -> r) -> r }

instance Monad (Eff r) where
  return a = Eff (\k -> k a)
  Eff f >>= m = Eff (\k -> f (\a -> runEff (m a) k))

instance Applicative (Eff r) where
  pure = return
  (<*>) = ap

instance Functor (Eff r) where
  fmap f c = c >>= \a -> return (f a)
~~~

The `Eff` monad has two type parameters. The second parameter is the
usual parameter all monads have which carries the result of the
computation. In itself the effect monad does not provide any
effects. New effects are adding by adding structure to the first
type parameter, the answer type of the continuation monad.

## Adding effects: The reader effect

The crucial idea in this paper it to model effects by adding structure
the answer type of the continuation monad. As a first concrete example
we will see how to do this to achieve an effect similar to the reader
monad.

~~~ {.haskell}
type Reader e r = e -> r

ask :: Eff (Reader e r) e
ask = Eff $ \k r -> k r r

local :: e -> Eff (Reader e r) a -> Eff (Reader e r) a
local e (Eff f) = Eff $ \k e' -> f (\a _ -> k a e') e

runReader :: Eff (Reader e r) a -> e -> Eff r a
runReader (Eff f) e = Eff $ \k -> f (\a _ -> k a) e
~~~

The reader effect takes two parameters. The first is the type of the
environment which is carried around. The other is the new answer type,
which can further be used to add new effects. The reader effect is
modelled by a function from the environment to the answer type. In
order to better comprehend the how this type implements a reader
effect, consider the full expanded type when the type `Reader` is
used as an effect:

~~~ {.haskell}
Eff (Reader e r) a = Eff ((a -> e -> r) -> e -> r)
~~~

Compared to monad transformers we don't have to define any monad
instance. It comes for free from using the continuation monad. 
We save ourselves quite a bit of code.

## The state and writer effect

The state effect and writer effect are implemented using the same type
as the reader effect. However, the operations and the way effects are
run differ.

~~~ {.haskell}
type State s r = s -> r

get :: Eff (State s r) s
get = Eff $ \k s -> k s s

set :: s -> Eff (State s r) s
set s = Eff $ \k s' -> k s' s

runState :: Eff (State s r) a -> s -> Eff r (a,s)
runState (Eff f) s = Eff $ \k -> f (\a s -> k (a,s)) s
~~~

The state effect provide the `get` and `set` functions for retrieving
and changing the state. They are implemented simply by passing the
correct state to the continuation. The function `set` returns the
old state as a result. When running the state effect the final state
is returned together with the result of the computation.

The writer effect uses a monoid to accumulate multiple writes. Writes
are performed using the `tell` function. The function `collect`
returns the current accumulated value.

~~~ {.haskell}
type Write w r = w -> r

tell :: Monoid w => w -> Eff (Write w r) ()
tell w = Eff $ \k w' -> k () (w' <> w)

collect :: Eff (Write w r) w
collect = Eff $ \k w -> k w w

runWriter :: Monoid w => Eff (Write w r) a -> Eff r (a,w)
runWriter (Eff f) = Eff $ \k -> f (\a w -> k (a,w)) mempty
~~~

## The error effect

The error effect is different from the previous effects that we have
seen so far. It is modelled using the standard method of adding an
extra error continuation.

~~~ {.haskell}
type Error e r = (e -> r) -> r

throw :: e -> Eff (Error e r) a
throw e = Eff $ \k k' -> k' e

catch :: Eff (Error e r) a -> (e -> Eff (Error e r) a) -> Eff (Error e r) a
catch (Eff f) h = Eff $ \k k' -> f k (\e -> runEff (h e) k k')

runError :: Eff (Error e r) a -> Eff r (Either e a)
runError (Eff f) = Eff $ \k -> f (\a k' -> k (Right a)) (\e -> k (Left e))
~~~

The functions for throwing, catching and running
error computations are standard.

## Lifting effects

So far, all the effectful operations that we've defined has assumed
that the effect the provide is the outermost effect in the effect
hierarchy. For instance, consider the type of the function `get`:

~~~ {.haskell}
get :: Eff (State s r) s
~~~

It assumes that the state effect is the outermost effect. But suppose
we wanted to have an effect hierarchy which looked like `Eff (Error e
(State s a))`. How can we access the state in this case? The key
insight into lifting effectful computations is to note that all
effects are of the form `T -> r` where the type `T` is different for
different effects. Since we know that the effect is modelled as a
function which returns the answer type we can write the following
lifting function:

~~~ {.haskell}
lift :: Eff r a -> Eff (e -> r) a
lift (Eff f) = Eff $ \k e -> f (\a -> k a e)
~~~

This function takes any effectful computation and lifts it one level
up the effect hierarchy. Note that the lift function works for all
effects, we don't have to define a separate function for each effect
as we have to do with monad transformers.

Using the function `lift` we can write programs like the following:

~~~ {.haskell}
test = flip runEff id $ flip runState 1 $ runError comp

comp :: Eff (Error String (State Int r)) Int
comp = catch
         (do lift $ set 7
             throw "foo")
         (\e -> return ())
~~~

The computation `comp` uses the `Error` effect and the `State` effect.
The computation is initialized with a state of 1. Within the scope of
the catch the state is set to `7` and an error is thrown. The error is
caught and discarded. The final state from the computation is still
`7` meaning that the state is preserved when throwing an
error.

# More type safety

The library presented so far suffers from type unsafety. The effects
state, reader and writer all look the same from the view of the type
system. That means that we can write programs like the following which
confuse the effects.

~~~ {.haskell}
unsafe = flip runEff id $ runWriter $
         do tell "foo"
            set "bar"
            env
~~~

The same effect is used as a writer effect, a state effect and a
reader effect. We would like to be able to rule out these kinds of
programs using the type system.

## A first attempt

A perhaps intuitive way of trying to solve the type unsafety issue is
to wrap effects in newtypes. For instance, we could write the reader
effect as follows:

~~~ {.haskell}
newtype Reader e r = Reader (e -> r)
~~~

However, this solution has the problem that we cannot write the
function `lift` uniformly for all effects. Moreover, there would be a
lot of packing and unpacking of the `Reader` constructor and we will
see how to get around that next.

## A better solution

Instead of wrapping the newtype around the whole effect type we will
be a little more careful. Remember that each effect is of the form `T
-> r`. In the previous section the type `T` was in many cases just a
type parameter, and this lead to the type unsafety problem that we're
trying to solve. The solution is then to have a new type `T` for each
effect. Here is how we can apply the solution to the reader effect:

~~~{.haskell}
newtype ReaderT e = ReaderT { getR :: e }
type Reader e r = ReaderT e -> r

env :: Eff (Reader e r) e
env = Eff $ \k r -> k (getR r) r

local :: e -> Eff (Reader e r) a -> Eff (Reader e r) a
local e (Eff f) = Eff $ \k e' -> f (\a _ -> k a e') (ReaderT e)

runReader :: Eff (Reader e r) a -> e -> Eff r a
runReader (Eff f) e = Eff $ \k -> f (\a _ -> k a) (ReaderT e)
~~~

Compared to the type unsafe version of the reader effect the above
code is nearly identical, apart from some uses of the `ReaderT`
constructor and the `getR` function. The upshot is now that the
functions on the state effect no longer can be used on the reader
effect since they work on a different type.

The solution is similar for all the other effects. We resist the
temptation to spelling out all the details.

## Overloading operations

In monad transformer library it is common to overload the effectful
operations to avoid having to insert calls to `lift` by hand. The same
can be done in our library now that we have the type safe interface in
place.

The type class capturing the reader effect looks as follows:

~~~{.haskell}
class ReaderM r where
  type Env r
  envC :: Cont r (Env r)
  localC :: (Env r) -> Cont r a -> Cont r a
~~~

The associated type `Env` is used to get the type of the environment
from the reader effect in the effect hierarchy.

The instance for the reader effect is trivial but serves to illustrate
the use of the type `Env`:

~~~ {.haskell}
instance ReaderM (ReaderT e -> r) where
  type Env (ReaderT e -> r) = e
  envC = env
  localC = local
~~~

The type `Env` projects the type of the environment from the reader
effect. The function `env` and `local` are those defined earlier for
the reader effect.

The instance for the state effect is more interesting. In particular,
lifting the state effect through the `local` function is non-trivial
since `local` takes an effectful computation as input. The code looks
as follows:

~~~ {.haskell}
instance ReaderM r => ReaderM (StateT s -> r) where
  type Env (StateT s -> r) = Env r
  envC = lift envC
  localC e (Eff f) = Eff $ \k s -> 
    runEff (localC e (Eff (\k' -> f (\a s -> k' a) s)))
      (\a -> k a s)
~~~

The crux when implementing `localC` is that the effectful computation
that is passed to the recursive call to `localC` doesn't use the state
effect, yet the argument `f` uses it. The solution is to adopt the
continuations by dropping the state or adding it where appropriately.

Overloading the operations for the other effects follow a similar
pattern. In the interest of succinctness we refrain from showing the
full code.

The way we've used type classes to overload effectful operations is
similar to what can be found in monad transformer libraries. It leads
to a quadratic number of instances. This many instances is a little
tedious but not really a problem in practice.

# Discussion

We have presented a new approach to compose effectful
computations. Similar to monad transformers but with distinct
advantages.

A downside of our model of effects is that lazy monads are not
representable. The continuation monad is strict and since we embed all
effects into it we have no way of making lazy monads.

## Future work

We have seen that our new model for composing effects seems similar to
monad transformers. An interesting piece of future work is to see
exactly what can be achieved within this framework and precisely how
it relates to monad transformers and algebraic effects.

We would also like to measure the performance of our library and see
how it compares to other approaches. We have good reasons to believe
that our library will perform well as monads written using
continuations tend to be fast, at least in Haskell.

Finally, we aim to take the ideas presented here and release a fully
fledged library for effectful programming.

## Related work

The closest related work is monad transformers [@liang1995monad],
which are well established among programmers as the way to compose
effects. We have already contrasted our approach to monad transformers
and will not repeat the comparison here.

Andrzej Filinski showed how to represent monad transformers using
continuations and imperative state [@filinski1999representing]. His
approach uses the state to store continuation and invoke them as
needed. The answer type in this work is a recursive type. The approach
presented in this paper can be seen as unrolling the recursion and
specializing the type depending on the effects used, keeping in mind
that we don't use any state to store continuations.

An alternative to monad transformers which has gained popularity
recently is algebraic effects. The algebraic effects framework allow
effects to be combined without determining upfront how the effects
should nest. The nesting is determined when the effects are
"run". This increases modularity compared to monad
transformers. However, algebraic effects cannot express all effects
that monad transformers can. The approach we have presented here is
closer to monad transformers in that the nesting of effects have to
be determined upfront.

# Bibliography