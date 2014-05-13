---
title: From semigroups to bind
author: João Cristóvão
tags: haskell , typeclass , monoid, semigroup, alt, apply, bind
---

_tl;dr:_ : what are Alt, Apply, Bind typeclasses

So, the one common comment I received on my first [haskell cheatsheet](http://fundeps.com/posts/cheatsheets/2014-03-04-cheat-sheets/)
was: it looks great, but I have no idea what half of those typeclasses are.

So, I guess an explanation is due.

My first glimpse of understanding came from this [beautiful ASCII art](http://hackage.haskell.org/package/semigroupoids),
which I'll reproduce here:


```
 Traversable <---- Foldable <--- Functor ------> Alt ---------> Plus           Semigroupoid
      |               |            |                              |                  |
      v               v            v                              v                  v
 Traversable1 <--- Foldable1     Apply --------> Applicative -> Alternative      Category
                                   |               |              |                  |
                                   v               v              v                  v
                                 Bind ---------> Monad -------> MonadPlus          Arrow

```

So, for the regular haskeller without a background in mathematics,this is what it is usually known:

__Monoid__:

```Haskell
class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a
```

__Functor__:

```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```


__Applicative:__

```Haskell
class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b
```

__Alternative__:

```Haskell
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a
```

So, what are all those new typeclasses?
--------------------------------------

#### First question: __what can I get if I remove the identity (from a Monoid)__?

You get a [__semigroup__](http://hackage.haskell.org/package/semigroups)
```Haskell
class Semigroup a where
  -- | An associative operation.
  (<>) :: a -> a -> a
```

_But, what does it do, that a monoid doesn't?_

It gives you [Min](http://hackage.haskell.org/package/semigroups-0.12.2/docs/Data-Semigroup.html#t:Min)
and [Max](http://hackage.haskell.org/package/semigroups-0.12.2/docs/Data-Semigroup.html#t:Max).
This would not be possible with a monoid. What's the identity of a Maximum? Do you want to define that?

Plus, you get more instances than you get with monoids,
namely  [Data.List.NonEmpty](http://hackage.haskell.org/package/semigroups-0.12.2/docs/Data-List-NonEmpty.html).
This is a list that is type checked to always have at least one element, and thus
functions like head always succeed. On the other hand, since there is no 'empty' element, this
type can only be an instance of Semigroup.

#### Second question: __what can I get if I remove the empty (from an Alternative)__?
##### Bonus question: __can I get something that behaves like an alternative, just with a Functor constraint?__

_Yes: It's called an [Alt](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Alt.html)_:

```Haskell
class Functor f => Alt f where
  -- | @(<|>)@ without a required @empty@
    (<!>) :: f a -> f a -> f a
```

_But, what does it do, that a alternative doesn't?_

It allows you to include some extra types, like [Either](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Either.html)
or IO. Thus, you can write stuff like

```Haskell
(...)
do
	-- IOAction1 `orElse` IOAction2
	xpto <- IOAction1 <!> IOAction2
```

#### Third question: __what can I get if I remove the pure from an Applicative__?

An [Apply](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Apply.html#t:Apply):

```Haskell
-- > associative composition: (.) <$> u <.> v <.> w = u <.> (v <.> w)
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b
```

_But, what does it do, that a applicative doesn't?_

It allows you to include some extra types, like [IntMap](http://hackage.haskell.org/package/containers-0.5.4.0/docs/Data-IntMap-Lazy.html)

#### Fourth question: __what can I get if I remove the return from a Monad__?

A [Bind](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Bind.html#t:Bind)

```Haskell
class Apply m => Bind m where
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = join (fmap f m)

  join :: m (m a) -> m a
  join = (>>- id)
```

_But, what does it do, that a monad doesn't?_

It allows you to include some extra types, like [IntMap](http://hackage.haskell.org/package/containers-0.5.4.0/docs/Data-IntMap-Lazy.html)

#### Fifth question: __Can I add a ```zero``` value to Alt__?

Well, this one does seem a little bit far fetched, doesn't it, since one of the steps of 
defining Alt was precisely to drop the empty... but, not just that (also just depending on
Functor, and not Applicative instances). Hence, ```zero``` arrives. 
Once again, you do gain some extra instances (for example):

```Haskell
instance Plus IO where
  zero = error "zero"

instance Ord k => Plus (Map k) where
  zero = Map.empty
```

I call it *the reliable empty*.


Conclusion
----------

See what I did here? Noticed the pattern?

As an important final note, while you might be mostly used to:

* [mempty](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#t:mempty)
  means empty
* [mappend](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#t:mappend)
  means appending
* Alternatives means choice

Those are assumptions that do not hold for all types (as I ilustrate on the second page
of the [cheatsheet](http://fundeps.com/tables/FromSemigroupToMonads.pdf) ).

In fact, once you start thinking about monoids/semigroups as simply *binary operations*, you start
seeing them everywhere, and you start appreciating the usefulness of sometimes dropping the 
identity/empty/return, etc.

[In my next post](/posts/haskell/2014-03-12-Monoids-Newtypes-and-HR/)
I will ilustrate this, and show how to automatically derive Monoid instances,
dramatically reducing your boilerplate.

As usual, please report any typo, error, or gross misundertanding on my part to the email below.
Thanks!

