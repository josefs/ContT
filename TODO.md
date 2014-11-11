# TODO

* I should add the non-determinism effect. 
  LogicT was very difficult. ListT should be easier. Where can
  I find an implementation which is OK.
* What about the continuation effect? And delimited effects?
* Should I be looking into Monad morphisms? I suppose I should
  be able to do something similar in my framework.
* Can I translate any monad to a monad transformer stack?
  What about the reverse?
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

LogicT is very difficult to get right in my framework it seems to
require higher order return types.

data Cont r a = Cont (forall b. ((a -> r b) -> r b))

My experiments in ContTT now seems to suggest that this type is the
solution. The tricky thing is getting the forall quantification in the
right place.

I initially thought that the type above was encodable in the standard
continuation monad which parameterises over the return type
(with kind *) but that is not the case

The problem with the version of Cont above is that the effects are
now partially applied since the return type is higher kinded.
This forces us to use newtypes, which may become a performance problem.
It is possible to write some effects using higher kinded type synonyms,
but that is not going to help us because the types they expand into
will have to be newtypes anyway, since they are not going to be
fully applied.

## Monatron

I now understand Monatron much better. It's basically an early
version of algebraic effects. The advantage is that we get
uniform liftings of monad transformers.

The question is what the difference is between Monatron and the
extensible effects library of Oleg and c:o.
