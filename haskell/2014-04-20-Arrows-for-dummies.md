---
title: Arrows for dummies
tags: haskell, Arrow, Either
---

_tl;dr:_ Useful arrow combinators everyone uses without really caring for arrows.

### Not about arrows

This post is not about arrows _per se_.

In fact, while arrows are seen as ["a new abstract view of computation (...) as monads (...) but more general"](http://www.haskell.org/arrows/),
many Haskell users _don't really care for this_. They just use _some arrow functions_ because they implement useful 
[Either](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html) 
and [Tuple](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Tuple.html) functions.

While there are already [good](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) arrow tutorials,
if you're a begginer, you'll probably gloss over them, and conclude they seem over complicated for what you are doing.

Heck, don't even get me started on the 
[Control.Arrow module documentation](http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html).

I remember thinking: let me get these monads straight, then monad transformers, those are also 
quite used, _then_ I'll consider arrows. And I argue that many haskellers never do get to that point...

### What am I missing?

One of Arrow instances is function application, or ```->```.

How is that useful? 

## Either

Many of you have probably wondered why there aren't functions on 
[base](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html)
to map the ```Left``` part of an Either, or even both ```Left``` and ```Right```.

You are [not](http://www.haskell.org/pipermail/libraries/2014-April/022679.html)
[alone](https://stackoverflow.com/questions/13503965/mapping-over-eithers-left).
My guess is that this question will just keep on poping until proper documentation is added to Data.Either.
Let's see if the [current effort](http://www.haskell.org/pipermail/libraries/2014-April/022777.html) 
provides any results.

While you might later find these defined in the very useful
module [either](http://hackage.haskell.org/package/either),
namely [Data.Either.Combinators](http://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html), 
as [mapLeft](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapLeft)
and [mapBoth](http://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapBoth),
what you might miss is that you don't require an extra package to get this: you can use ```Control.Arrow``` from base!


```Haskell
-- mapLeft
left :: (b -> c) -> Either b d -> Either c d

-- mapRight / fmap
right :: (b -> c) -> Either d b -> Either d c

-- mapBoth
(+++) :: (b -> c) -> (b' -> c') -> Either b b' -> Either c c'

-- either
(|||) :: (b -> d) -> (c -> d) -> Either b c -> d
```

## 2-Tuple (Pair)

Another usefull arrow combinators usage is Tuples:

```Haskell
-- Apply function to first pair element
first :: (b -> c) -> (b,d) -> (c,d)

-- fmap (Apply function to second pair element)
second :: (b -> c) -> (d,b) -> (d,c)

-- Apply functions to both elements
(***) :: (b -> c) -> (b' -> c') -> (b,b') -> (c,c')

-- Value to pair (fanout)
(&&&) :: (b -> c) -> (b -> c') -> b -> (c,c')
```

## Cheatsheet

So, arrows operators can be quite useful for types that are parameterized over two types:
[Either](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Either.html) for choice,
[Tuple](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Tuple.html) for combinations.

<table class="table table-striped">
  <thead>
	<tr>
		<th>Apply function to	</th>
		<th>Type				</th>
		<th>Arrow<br />Operator	</th>
		<th>Alternative<br />in Base</th>
		<th>Alternative<br />in [either](https://hackage.haskell.org/package/either)</th>
		<th>Alternative<br />in [bifunctor](http://hackage.haskell.org/package/bifunctors)</th>
		</tr>
  </thead>
  <tbody class="table-striped">
	<tr><td>First type	</td><td>Either	</td><td>left	</td>
		<td></td>
		<td>[mapLeft](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapLeft)</td>
		<td>[first](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:first)</td>
	</tr>
	<tr><td>			</td><td>(,)	</td><td>first	</td>
		<td></td>
		<td></td>
		<td>[first](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:first)</td>
	</tr>
	<tr><td>Second type	</td><td>Either	</td><td>right	</td>
		<td>fmap</td>
		<td></td>
		<td>[second](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:second)</td>
	</tr>
	<tr><td>			</td><td>(,)	</td><td>second	</td>
		<td>fmap</td>
		<td></td>
		<td>[second](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:second)</td>
	</tr>
	<tr><td>Both types	</td><td>Either	</td><td>(+++)	</td>
		<td></td>
		<td>[mapBoth](https://hackage.haskell.org/package/either-4.1.1/docs/Data-Either-Combinators.html#v:mapBoth)</td>
		<td>[bimap](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:bimap)</td>
	</tr>
	<tr><td>			</td><td>(,)	</td><td>(***)	</td>
		<td></td>
		<td></td>
		<td>[bimap](http://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html#v:bimap)</td>
	</tr>
  </tbody>
</table>

## Examples, please!

### Modify both elements of a tuple

<small style="color:brown">the following examples can be copy-pasted into ghci</small>

Lets say you've got a string that you want to quickly split into two, and process each part independently.
You could use [attoparsec](http://hackage.haskell.org/package/attoparsec), but it is just a one time use,
and you would prefer to avoid adding any other module.

So, you start by using [```break```](http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:break):

```Haskell
let x 		  = "1_test"
let splitPair = break (=='_') x	-- ("1","_test")
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
let numStr :: (Int,String) ; numStr = read +++ drop 1 $ splitPair
```

Much cleaner.

### Change the Left type of a Either

Since a Monad takes a type constructor with exactly one type, 
while there is a Monad instance for Maybe, there isn't one for ```Either``` alone:
you must specify the ```Left``` type, thus making it mandatory.

This means that if you have three ```Either``` returning functions:

```Haskell
import Data.Text
import Data.AttoParsec.Text

readInt       :: Text -> Either String Int
readDouble    :: Text -> Either String Double
addToInt      :: Int  -> Double -> Either Double Int

readInt      = parseOnly decimal
readDouble   = parseOnly double

addToInt i d = let
	lossyInt :: Int 
	lossyInt = truncate d 
	in if fromIntegral lossyInt == d 
	       then Right $ lossyInt + i
		   else Left  $ fromIntegral i + d


## But how?



