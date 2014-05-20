---
title: Not about arrows
tags: haskell, Arrow, Either, cheatsheet
---

_tl;dr:_ Useful arrow combinators everyone uses without really caring for arrows.

Or ***arrow combinators for dummies***.

<!---
***Index***:

* [Not about arrows](#not-about-arrows)
* [What am I missing?](#what-am-i-missing)
    - [Either](#either)
    - [Tuple] (#tupple-pair)
* [Examples, please!](#examples-please)
* [But how does this work?](#how-does-this-work)
* [Is this the right abstraction?](#is-this-the-right-abstraction]
* [Cheatsheet](#cheatsheet)
-->

## About

This post is not about arrows _per se_.

In fact, while arrows are seen as ["a new abstract view of computation (...) as monads (...) but more general"](http://www.haskell.org/arrows/),
many Haskell users _don't really care for this_. They just use _some arrow functions_ because they implement useful 
[Either](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html) 
and [Tuple](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Tuple.html) combinators.

While there are already [good](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) arrow tutorials,
if you're a beginner, you'll probably gloss over them, and conclude they seem over complicated for what you are doing.

Heck, don't even get me started on the 
[Control.Arrow module documentation](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html).

I remember thinking: let me get these monads straight, then monad transformers, those are also 
quite used, _then_ I'll consider arrows. And I argue that many haskellers never do get to that point...

## What am I missing?

One of Arrow instances is function application, or ```->```.

How is that useful? 

### Either

Many of you have probably wondered why there aren't functions on 
[base](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html)
to map the ```Left``` part of an Either, or even both ```Left``` and ```Right```.

You are [not](http://www.haskell.org/pipermail/libraries/2014-April/022679.html)
[alone](https://stackoverflow.com/questions/13503965/mapping-over-eithers-left).
My guess is that this question will just keep on popping until proper documentation is added to Data.Either.

While you might later find these defined in the very useful
module [either](http://hackage.haskell.org/package/either),
namely [Data.Either.Combinators](http://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html), 
as [mapLeft](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapLeft)
and [mapBoth](http://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapBoth),
what you might miss is that you don't require an extra package to get this: you can use ```Control.Arrow``` from base!


```Haskell
-- Apply a function  to the left side of an Either
-- The same as 'mapLeft' from Data.Either.Combinators
left :: (b -> c) -> Either b d -> Either c d

-- Apply a function to the right side of an Either
-- the same as 'mapRight' from Data.Either.Combinators
-- but you should just used 'fmap' instead
right :: (b -> c) -> Either d b -> Either d c

-- Apply one of the provided functions to the Either value,
-- resulting from the case analysis.
-- The same as 'mapBoth' from Data.Either.Combinators
(+++) :: (b -> c) -> (b' -> c') -> Either b b' -> Either c c'

-- Case analysis.
-- The same as 'either'
(|||) :: (b -> d) -> (c -> d) -> Either b c -> d
```

### 2-Tuple (Pair)

Another useful set of arrow combinators applies to Tuples:

```Haskell
-- Apply a function to first pair element
first :: (b -> c) -> (b,d) -> (c,d)

-- Apply a function to second pair element
-- but you should just use 'fmap' instead
second :: (b -> c) -> (d,b) -> (d,c)

-- Apply the first function to the first element,
-- and the second function to the second element.
(***) :: (b -> c) -> (b' -> c') -> (b,b') -> (c,c')

-- Value to pair (fanout)
(&&&) :: (b -> c) -> (b -> c') -> b -> (c,c')
```

## Examples, please!

### Modify both elements of a tuple

<small style="color:brown">the following examples can be copy-pasted into ghci</small>

Lets say you've got a string that you want to quickly split into two, and process each part independently.
You could use [attoparsec](http://hackage.haskell.org/package/attoparsec), but perhaps for a one time use
you would prefer to avoid adding any other module.

So, you start by using [```break```](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:break):

```Haskell
let x           = "1_test"
let splitPair = break (=='_') x    -- ("1","_test")
``` 

And now, you would like to ```read``` the first tuple element, and drop the underscore from the second.
Usually, you would do this:

```Haskell
let numStr :: (Int,String) ; numStr = (\(a,b) -> (read a, drop 1 b)) splitPair
```

And you get the intended ```(1,"test")```

Now, how would you do this with arrows?

```Haskell
import Control.Arrow
(...)
let numStr :: (Int,String) ; numStr = read *** drop 1 $ splitPair
```

Much cleaner.

### Change the Left type of a Either

Since a ```Monad``` takes a type constructor with exactly one type, 
it is not possible to provide a ```Monad``` instance for ```Either``` alone.
You must specify its ```Left``` type (hence fixing it).

A simple example:

<div class="alert alert-danger">
```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Data.Text
import Control.Arrow
import Control.Applicative

-- Lets say most of our code is on the Either Text Int monad...
f :: Int -> Either Text Int
f x = if x > 0 then Right x
               else Left "Not bigger than 0"

-- But by an unfortunate event, we need to use a function
-- that returns a different Left type
g :: Int -> Either String Int
g y = if y `mod` 3 == 0 then Right y
                        else Left "Not divisible by 3"

eitherMonad :: Int -> Int -> Int -> Int -> Either Text Int
eitherMonad x y w z = do
  a <- f x
  b <- g y -- it fails here! g is a different monad!
  c <- f w
  d <- f z
  return $ a * b + 2 * c + d

main = do
  print $ eitherMonad 1 2 3 4 -- not divisible by 3
  print $ eitherMonad 1 9 0 4 -- not bigger than 0
  print $ eitherMonad 0 1 3 4 -- not divisible by 3
                              -- it only reports the first left
  print $ eitherMonad 1 9 3 4 -- it should return 19
                              -- but it doesn't, because this 
                              -- code does not compile
```
</div>

Using ```left```, we can modify the ```Left``` side of the ```Either```,
and thus keep working in the same monad:

<div class="alert alert-success">
```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Data.Text
import Control.Arrow
import Control.Applicative

-- Lets say most of our code is on the Either Text Int monad...
f :: Int -> Either Text Int
f x = if x > 0 then Right x
               else Left "Not bigger than 0"

-- But by an unfortunate event, we need to use a function
-- that returns a different Left type
g :: Int -> Either String Int
g y = if y `mod` 3 == 0 then Right y
                        else Left "Not divisible by 3"

eitherMonad :: Int -> Int -> Int -> Int -> Either Text Int
eitherMonad x y w z = do
  a <- f x
  b <- left pack $ g y -- it now works!
  c <- f w
  d <- f z
  return $ a * b + 2 * c + d

main = do
  print $ eitherMonad 1 2 3 4 -- not divisible by 3
  print $ eitherMonad 1 9 0 4 -- not bigger than 0
  print $ eitherMonad 0 1 3 4 -- not divisible by 3
                              -- it only reports the first left
  print $ eitherMonad 1 9 3 4 -- OK! returns 19
                                                                                                                               
```
</div>


## But how does this all work?

The strange thing is when you look at the functions signatures in ```Control.Arrow```,
they do not seem applicable:

```Haskell

-- | Feed marked inputs through the argument arrow,
--   passing the rest through unchanged to the output.
left :: a b c -> a (Either b d) (Either c d)

-- | A mirror image of left.
right :: a b c -> a (Either d b) (Either d c)

-- | Split the input between the two argument arrows,
--   retagging and merging their outputs.
(+++) :: a b c -> a b' c' -> a (Either b b') (Either c c') 

-- | Fanin: Split the input between the two argument arrows 
--   and merge their outputs.
(|||) :: a b d -> a c d -> a (Either b c) d 
```

However, if you consider that function application, also represente as an ```->```,
is an instance of ```Arrow```, things get more clear... How? Just replace
```a``` with ```->```:

```Haskell

-- | Feed marked inputs through the argument arrow,
--   passing the rest through unchanged to the output.
left :: a    b c -> a    (Either b d)  (Either c d)
     :: (->) b c -> (->) (Either b d)  (Either c d) 
     :: (b -> c) ->       Either b d -> Either c d

-- | A mirror image of left.
right :: a    b c -> a    (Either d b)  (Either d c) 
      :: (->) b c -> (->) (Either d b)  (Either d c) 
      :: (b -> c) ->       Either d b -> Either d c

-- | Split the input between the two argument arrows,
--   retagging and merging their outputs.
(+++) :: a    b c -> a    b' c' -> a    (Either b b')  (Either c c') 
      :: (->) b c -> (->) b' c' -> (->) (Either b b')  (Either c c') 
      :: (b -> c) -> (b' -> c') ->       Either b b' -> Either c c'

-- | Fanin: Split the input between the two argument arrows 
--   and merge their outputs.
(|||) :: a    b d -> a    c d -> a    (Either b c)   d 
      :: (->) b d -> (->) c d -> (->) (Either b c)   d
      :: (b -> d) -> (c -> d) ->       Either b c -> d
```

And the same applies for Pairs:


```haskell
-- | send the first component of the input through the argument 
-- arrow, and copy the rest unchanged to the output.
first :: a    b c -> a    (b, d)   (c,d)
      :: (->) b c -> (->) (b,d)    (c,d)
      :: (b -> c) ->      (b,d) -> (c,d)
     
-- | a mirror image of first.
second :: a    b c -> a     (d, b)    (d, c)
       :: (->) b c -> (->)  (d, b)    (d, c)
       :: (b -> c) ->       (d, b) -> (d, c)


-- | split the input between the two argument arrows
--   and combine their output.
(***) :: a    b c -> a    b' c' -> a    (b, b')    (c, c')
      :: (->) b c -> (->) b' c' -> (->) (b, b')    (c, c')
      :: (b -> c) -> (b' -> c') ->      (b, b') -> (c, c')

-- | fanout: send the input to both argument arrows
--   and combine their output.
(&&&) :: a    b c -> a    b c' -> a    b    (c, c')
      :: (->) b c -> (->) b c' -> (->) b    (c, c')
      :: (b -> c) -> (b -> c') ->      b -> (c, c')
```

## Is this the right abstraction?

The 
[current](http://www.haskell.org/pipermail/libraries/2014-April/022777.html) 
[effort](http://www.haskell.org/pipermail/libraries/2014-April/022844.html)
to add more straight forward functions to ```Data.Either``` and ```Data.Tuple```
ended with the conclusiong that
[Bifunctors](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html) should be added to base.

Bifunctors are really nice, and a much more (IMHO) logical way to present these operators. 
Take any given type constructor that parameterizes over two types, make it pass some very obvious rules (```bimap id id ≡ id```),
and you got yourself something that it is just like a ```Functor```, but for "bi-types".

So, ```Either``` is an instance, and so is ```(,)```.
And the equivalent functions are the very obvious:

```Haskell
class Bifunctor p where
    -- |  Map over both arguments at the same time.
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

    -- | Map covariantly over the first argument.
    first :: (a -> b) -> p a c -> p b c

    -- | Map covariantly over the second argument.
    second :: (b -> c) -> p a b -> p a c
```

While this is, IMHO, not a bad outcome at all, 
I still feel that more straight-forward functions on ```Data.Either``` and ```Data.Tuple``` would do more good than harm,
namely easing the way for Haskell newbies.

But the ```Bifunctor``` is a very useful (and logical) abstraction, and it is definitely most welcomed.

## Cheatsheet

So, arrows operators can be quite useful for types that are parameterized over two types:
[Either](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html) for choice,
[Tuple](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Tuple.html) for combinations.

<table class="table table-striped">
  <thead>
    <tr>
        <th>Apply function to    </th>
        <th>Type                </th>
        <th>Arrow<br />Operator    </th>
        <th>Alternative<br />in Base</th>
        <th>Alternative<br />in [either](https://hackage.haskell.org/package/either)</th>
        <th>Alternative<br />in [bifunctor](http://hackage.haskell.org/package/bifunctors)</th>
        </tr>
  </thead>
  <tbody class="table-striped">
    <tr><td>First type    </td>
		<td>Either    </td>
		<td>[left](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:left)</td>
        <td></td>
        <td>[mapLeft](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapLeft)</td>
        <td>[first](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:first)</td>
    </tr>
    <tr><td>            </td>
		<td>(,)    </td>
		<td>[first](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:first)</td>
        <td></td>
        <td></td>
        <td>[first](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:first)</td>
    </tr>
    <tr><td>Second type    </td>
		<td>Either    </td>
		<td>[right](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:right)</td>
        <td>[fmap](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:fmap)</td>
        <td>[mapRight](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapRight)</td>
        <td>[second](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:second)</td>
    </tr>
    <tr><td>            </td>
		<td>(,)    </td>
		<td>[second](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:second)</td>
        <td>[fmap](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:fmap)</td>
        <td></td>
        <td>[second](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:second)</td>
    </tr>
    <tr><td>Both types    </td>
		<td>Either    </td>
		<td>[(+++)](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:-43--43--43-)</td>
        <td></td>
        <td>[mapBoth](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapBoth)</td>
        <td>[bimap](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:bimap)</td>
    </tr>
    <tr><td>            </td>
		<td>(,)    </td>
		<td>[(***)](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:-42--42--42-)</td>
        <td></td>
        <td></td>
        <td>[bimap](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:bimap)</td>
    </tr>
  </tbody>
</table>


## Function composition

Finally, there is yet another gem hidden in the ```->``` instance of ```Arrow```: function composition.

```Haskell
-- | Left-to-right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c 

-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c 
```

Where again, replacing ```cat``` with ```->``` gets you the expected result.
However, once again, there is a better abstraction for this.

If you only need left-to-right function composition, you should probably just import
[Control.Category](http://hackage.haskell.org/package/base-4.6.0.0/docs/Control-Category.html), which has been on ```Base```
at least since version 4.0.


## Conclusion

I hope you have liked this introductory text. Please let me know what you think, and if you found any error or typo.
None of this is especially new, but my intent was to join all this information in one place.
If there is an equivalent guide somewhere else, please let me know.

Cheers,
João



