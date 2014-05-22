---
title: An experiment with typed time
tags: haskell, type families, time, units
---

_tl;dr:_ Using the [units](http://en.wikipedia.org/wiki/Real-time_operating_system) package for correctly specifying delays

## Motivation

So, I was doing some experiences with the
[Retry](http://hackage.haskell.org/package/retry) module, that accepts
a base delay as an ```Int``` in _milliseconds_.
Then, I was testing this with ```threadDelay``` which accepts _microseconds_.

But the code I was working on actually required delays in the order of seconds,
and perhaps even minutes.

As you can now probably guess, this is asking for trouble.

## Naive solution

Well, I could just define functions like these:

```Haskell
-- Convert seconds to milliseconds
secondsToMs :: Int -> Int
secondsToMs = (*1000)

-- Convert seconds do microseconds
secondsToUs :: Int -> Int
secondsToUs = (*1000000)
```

But this is still asking for trouble.
Nothing is stopping me from mixing seconds with milliseconds with microseconds.
Besides, having `toUs` just feels wrong :P

## Libraries ahoy

Looking in hackage for more type safe solutions yields a lot of libraries.
I'll focus first in two time specific libraries:

### [tiempo](https://hackage.haskell.org/package/tiempo)

Tiempo, found [here](https://hackage.haskell.org/package/tiempo) seems to be just a proof-of-concept on
expanding the implementation I presented above.

You do get a specific type, ```TimeInterval```, and functions to convert to and from.

However, its interface is far from uniform:

```Haskell
toMicroSeconds :: TimeInterval -> Int
toMilliSeconds :: TimeInterval -> Double
toSeconds      :: TimeInterval -> Double
```

While I can see why they did this - some base functions dealing with delays use ```Int``` as microseconds -,
it doesn't feel clean.

Also, there is no ```Num``` instance, thus you can't even add or subtract ```TimeInterval```s.

However, there is one thing I like in this library: it redefines the
```threadDelay``` and ```timeout``` functions to accept a TimeInterval,
which is one of my goals.


### [time-units](https://hackage.haskell.org/package/time-units)

This library, found [here](https://hackage.haskell.org/package/time-units), seems to get
some things right. A ```Num``` instance makes it easy to declare values:

```Haskell
import Data.Time.Units

let x = 5 :: Second
print x
> 5s
```
And there are separate data types for seconds, minutes, etc, so you can't accidentally
mix them up. You can use the ```convertUnit``` to perform (sometimes lossy) conversions.

But, this does imply a somewhat bulky way of adding up times:

```Haskell
import Data.Time.Units

let x = convertUnit ((5 :: Millisecond) + convertUnit (6 :: Second)) :: Microsecond
> 6005000µs
```

## General unit libraries

Next, I finally stumbled on 
[www.haskell.org/haskellwiki/Physical_units](http://www.haskell.org/haskellwiki/Physical_units).
There are _lots_ of libraries for achiving this, and to be honest,
I was a little bit lost.

And if you are hopping for a detailed technical explanation regarding my 
following choice, I am afraid I don't have one. I just choose one which
seemed to be actively developed, and was mainly focused on unit support.
And, of course, that was usable by a dumb guy like me.

Thus, I ended up choosing [units](http://www.cis.upenn.edu/~eir/packages/units/) 
over [unittyped](http://hackage.haskell.org/package/unittyped) 
mainly because the later only has one release in Hackage.

### [Units](http://hackage.haskell.org/package/units)

This library is actually split in two:

* [units](http://hackage.haskell.org/package/units) : implements all the Units 
    type hackery. Its type signatures do seem a little daunting, but if you 
    stick with the functions I'll describe next, you should not have a problem.
* [units-defs](https://hackage.haskell.org/package/units-defs) : The above 
    library does not actually define any Unit besides the very generic Scalar. 
     But you can import units-defs to have access to most (all?) units you 
     might want.

#### Minimum viable set of functions

So, you want to use time. Fine:

```Haskell
import Data.Metrology
import Data.Metrology.SI
import Data.Metrology.Show

let x = 5 %% Second
let y = 6 %% milli Second
let z = x |+| y 
z
> 5.006 s
```

Looks great! What if I want the results in microseconds?

```Haskell
let ms = z ## micro Second
> 5006000.0
```

Even better! As you can see, with this solution you can freely mix different 
values of the same physical quantity, and you always get a correct result.
And you can easily convert the final result to the format of your preference.

While there is no ```Num``` instance, the ```|+|```, ```|*|```, etc 
operators do everything you might need.

Thus, while the type signatures do seem a little daunting 
(and type inference might not always work), 
the actual use is quite easy!

A notorious disadvantage of this solution is that it uses 
[closed type families](http://www.haskell.org/haskellwiki/GHC/Type_families#Closed_family_simplification),
and thus [it only supports GHC 7.8.2+](https://github.com/goldfirere/units/issues/1). 
But if you are not bound to a specific GHC version,
this is not a major limitation.

#### Implementating threadDelay

So, can we implement a threadDelay which accepts ```Time``` as the delay unit?

Yes, let me show you how:

```Haskell
{-# LANGUAGE TypeFamilies  #-}
module Control.Concurrent.Units
  ( threadDelay
  , milli
  , micro
  ) where

import qualified Control.Concurrent as Conc
import Data.Metrology
import Data.Metrology.SI

-- | Like '##', but it performs a lossy conversion to Integral
-- using truncate. Use with care.
(#!) :: ( ValidDLU dim DefaultLCSU unit
        , Fractional n, RealFrac n, Integral i )
     => Qu dim DefaultLCSU n -> unit -> i
a #! b = round (a ## b)
infix 5 #!

-- | Modified thread delay that accepts Time.
threadDelay :: Time -> IO ()
threadDelay t = Conc.threadDelay ( t #! micro Second)
```

So, the only thing that deserves some explanation here is the ```#!``` operator.
It's quite simple really: I just copied the ```##``` type signature, but modified
it to return ```Integral``` types (thus supporting ```Int```) at an expense of
precision, since I am using ```round```.

Having defined this function, the ```threadDelay``` function definition is very simple.

#### And what about minutes and hours

Well, the SI does not define minutes and hours, only seconds.
And lets face it, nobody uses ```kilo seconds```.

So, lets add them to our little module:

```Haskell
data Minute = Minute
instance Unit Minute where
  type BaseUnit Minute = Second
  conversionRatio _ = 60
instance Show Minute where
  show _ = "min"

data Hour = Hour
instance Unit Hour where
  type BaseUnit Hour = Second
  conversionRatio _ = 60 * 60
instance Show Hour where
  show _ = "hour"
```

This is just code copied from [units-defs](https://hackage.haskell.org/package/units-defs)
Originally I defined ```Minute``` and ```Hour``` 
as Prefixes, but ```5 %% hour Second``` just didn't feel right.
With the above implementation, I can just type:

```Haskell
threadDelay (2.5 %% Minute)
```

#### Is this a good solution?

Well... It is a bit overkill, to be honest.
And don't get illusions of precision when dealing with time, 
and ```threadDelay```: this is not a 
[RTOS](http://en.wikipedia.org/wiki/Real-time_operating_system). 
Please note that this has nothing to do
with the units package precision, but the actual use of precise 
time in a non-real-time operating system as Linux, Windows or OSX.

What this solution **does** allow you is to think naturally in 
whatever units you prefer, mix them, 
and have the compiler do the dirty work for you.

And for me, that's a good enough advantage!



