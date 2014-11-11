# TODO

* I should add the non-determinism effect. 
  LogicT was very difficult. ListT should be easier. Where can
  I find an implementation which is OK.
* What about the continuation effect? And delimited effects?
* Measurements. Agains mtl, transformers and algebraic effects
  * Against hand-rolled monad like Parsec
  * Try out the examples in the LogicT paper
  * Against hand-rolled which doesn't use continuations
* Implement all the variations, including overloading.
* Write paper.

Maybe:

* A template for ACM papers

# Notes

* These days, mtl depends on transformers
* transformers contains quite a lot of stuff, including:
  * functors and functor composition
  * applicative backwards and lift
  * monad transformers for a lot of things
    * Cont
    * Error
    * Except
    * Identity
    * List (which is somewhat broken)
    * Maybe
    * RWS (sigh...)
    * Reader
    * State
    * Writer
    * Lazy and strict versions of RWS, State and Writer

Can I model lazy state and writer with my approach? Looking at the
types of the CBN cps translation (e.g. in "CPS transformation after
Strictness Analysis, by Danvy and Hatcliff) is seems hard to use the
Continuation monad to model laziness or CBN. We have to transform
argument to become thunks but a monad only takes care of transforming
results.

However, we live in Haskell, which is lazy, so maybe we don't have
to create thunk from arguments...

## LogicT

LogicT is very difficult to get right in my framework and might
require higher order return types.

data Cont r a = Cont (forall b. ((a -> r b) -> r b))

I haven't yet worked it out but it might be that I need to do
something like that in order for the types to work out. The tricky
thing is getting the forall quantification in the right place.

I initially thought that the type above was encodable in the standard
continuation monad which parameterises over the return type
(with kind *) but that is not the case

## Monatron

I now understand Monatron much better. It's basically an early
version of algebraic effects. The advantage is that we get
uniform liftings of monad transformers.
