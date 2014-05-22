---
title: All about functions
author: João Cristóvão
tags: haskell , functions , cheatcheets
---

Data.Function
-------------

A (hopefully) clear and extensive explanation of all there is
to know (on an intermediate level) regarding functions in Haskell.

This is a very practical guide. 
You will not see references to theory jargon unless strictly necessary 
(i.e., probably never).

This post is my rendering/opinion of some of the documentation that
should be on [Data.Function](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Function.html),
namely functions provided and documentation.

The expected level of familiarity with Haskell for this ghide is that of 
[Learn You a Haskell for Great Good](http://learnyouahaskell.com/higher-order-functions).

Functions in Haskell
--------------------

If you are familiar with the very basics of functions in Haskell, feel free to 
[skip this section]().

### Basics (Part I)

Function syntax, in Haskell, is _simpler_ than in other languages.
However, it may take you some time to familiarize yourself with it.

Lets take a simple example, a custom function to add two numbers, 
and duplicate the result, called ```addNDouble```.
Lets first see how we would use such a function:

In Python:

```Python
addNDouble (4,5)
> 18
```

In Haskell:
```Haskell
addNDouble 4 5
> 18
```

As you can see, it is not necessary to enclose the set of parameters over parenthesis,
neither to separate them with commas: spaces will do just fine.

Of course, this syntax choice means you might need to use parenthesis nevertheless 
to evaluate more complex expressions in a given parameter, 
where in Python this would not be needed.

```Python
addNDouble (a-b,5)
```

```
addNDouble (a-b) 5
```

### Basics (Part II) | Partial application

In Haskell functions can be partially applied, to get new functions:

```Haskell
let partial = addNDouble 5
```

```partial``` is a new function that has one parameter, adds 5 to it and then
doubles the result. We'll get back to this later.

### Basics (Part III) | Defining functions

Defining the function above in Python would be something like:

```Python
def addNDouble (x,y):
	return ((x+y)*2)
```

In Haskell, the simplistic way to do it would be:
```Haskell
addNDouble x y = (x + y) * 2
```

So, its actually simpler than the Python declaration.

But, as you might alread know, Haskell is a strongly typed language, and
thus the function above is not that well specified, since we don't indicate
what types it should operate on.

In fact, for a Haskell compiler:

**Int ≠ Integer ≠ Float ≠ Double**

And no, you can not freely mix them.
You'll get a type error!

While this is might seem a major bummer comming from dynamic languages,
it is really one of the major advantages of Haskell. _You'll always know what you get_.
(But as this is not the main focus of this guide, I will not pursue this further).

But one thing is clear. While the compiler can, much of the time, guess the types
being used, you should, whenever obvious, specify them, 
**specially if you are starting with Haskell**.

It will help you reason about your code, and it will help you avoid
confusing error messages.

So, let's redefine our function to operate on ```Int```.


```Haskell
addNDouble :: Int -> Int -> Int
addNDouble x y = (x + y) * 2
```

Or, if you are trying this out in GHCi (notice the valid ```;```):

```Haskell
let addNDouble :: Int -> Int -> Int ; addNDouble x y = (x + y) * 2
```

So, two lines as in Python. But what do I get in return?
The safety of knowing that if you try to apply this function to a ```String```,
the compiler will generate an Error, thus stopping you of ever 
getting this fantastic code to production level.


### Basics (Part IV) | Partial application (Part II)

The type signature of the function tells us a lot.
```->``` means _function that takes a parameter with the type at its left 
and returns a result with the type at its right_.

So, what does this mean? It means that these two are equivalent:

```Haskell
addNDouble :: Int  ->  Int -> Int
addNDouble :: Int  -> (Int -> Int)
```

But this is a different function:

```Haskell
addNDouble' :: (Int -> Int) -> Int
```

How is this so?

In fact, **every function in Haskell is indeed a function that just takes one
argument, and returns another (single) argument**. This argument may be a value, 
or another function. Thus, applying a single ```Int``` to our beloved
```addNDouble``` function just returns a new function, with type ```Int -> Int```.

So, this gets evaluated as:

```Haskell
addNDouble 5 6 => (addNDouble 5) 6
                       ↑
      		   new function of type
			   :: Int -> Int
```

But wait, wait, wait... isn't that just like this:

```Haskell
addNDouble' :: (Int -> Int) -> Int
```

Which I just stated that was not equivalent (to ```Int -> Int -> Int```).

If you're feeling confused, notice this: one thing is how Haskell evaluates 
each parameter, and turns it into a new value. It just so happens that it does 
so processing just one parameter at the time, and thus returning 
a new function or a value.  Actually, the previous sentence is not entirely accurate:
in Haskell, **functions can be seen as values**. 
So, a function always returns a value, that can be a simple type (like ```Int```), 
or a function.

On the other hand, if you pass a parameter to a function that has the type 
```(Int -> Int)``` (notice the parenthesis),
then you are passing a **single parameter** that is a function.

Thus, a valid definition for ```addNDouble'``` would be:

```Haskell
-- Not really addNDouble, impossible with this type signature
addNDouble' :: (Int -> Int) -> Int
addNDouble' f = (f 5) * 2
```


### Basics (Part V) | Precedence and fixity (Part I)

What do you get when you write:

```Haskell
addNDouble 4 5 + 3
```

The lack of parenthesis can make this slightly confusing, so it is important 
to know the rules.
Function application **always** takes precedence over operators, so the previous
expression always evaluates ```addNDouble 4 5``` first.

Thus, function application obeys the following rules:

* It binds to the right (relative to their parameters)
* It has higher precedence (10) then anything else, namely operators

Operators are functions too. They are not Haskell reserved words.
However, they have some differences regarding 'alphanumeric functions', namely:

* Their precedence level can be specified, but it is always less than regular function application (0..9).
* They can be made left,right or non-associative.

Some examples,from GHCI:

```Haskell
:i (*)
class Num a where -- consider a = Int, if it helps
(*) :: a -> a -> a
infixl 7 *

:i (+)
class Num a where -- consider a = Int, if it helps
(+) :: a -> a -> a
infixl 6 +

:i (^)
class Num a where -- consider a = Int, if it helps
(^) :: (Num a, Integral b) => a -> b -> a -- and b = Int
infixr 8 ^
```

Thus, the equation:

```Haskell
1 + 2 ^ 3 * 4
> 33
```

Yelds the expected result, ```^``` gets evaluated first, next the multiplication with 4, finally the sum with 1. 


### Basics (Part VI) | Non-operator functions as operators

You can take a _non-operator_ function and apply it as an operator, using backticks.
Taking our familiar example, the following two lines are equivalent, yielding the same result:

```Haskell
addNDouble 4 5
4 `addNDouble` 5
```

However, one might get confused with the result of the following function:

```Haskell
addNDouble 4 5 `addNDouble` 2
> ???
```

It is important to note that a _function turned operator_ **loses** its 10 precedence,
and gets attributed (by default) a left precedence of 9.
So, the 5 in the above expression is consumed by the first addNDouble, and its result is then feed as the first 
parameter of the second addNDouble. The result is, as you might have guessed, ```40```.

To make this cristal clear, some extra examples:

```Haskell
4 `addNDouble` 5 + 6 -- `addNDouble` has implicit infixl 9
> 24
```

```Haskell
infixl 5 `addNDouble`
4 `addNDouble` 5 + 6 -- (++) has infixl 6
> 30
```

_Yes, you can also give a fixity to backtick'ed regular (non-operator) functions._

### Basics (Part VII) | Operators

Operators are functions that:

* Are usually aplied in an infix way: ```5 + 6```
* Have names based exclusively on certain symbols (!#$%&*+./<=>?@\^-~ and unicode symbols).
  [(reference)](http://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators)
* Can also be applied in a non-infix way by surrounding them with parenthesis.

This last point is worth an example:

```Haskell
-- The following two sentences are equivalent:
let x = 5 + 6
let x = (+) 5 6
```

But the following is wrong (on its own):


<div class="alert alert-danger">

```Haskell
-- Invalid!
let x = 5 (+) 6
```

</div>


<div class="alert alert-warning">
**Note:** While symbolic operators cannot feature any regular alphanumeric
caracter on their name, **they can be used qualified**.

```Haskell
import qualified Prelude as P

let x = a P.+ b		-- this is valid (no parenthesis required)
```
</div>


### Basics (Part VIII) | Post-Fix Operators

We've already discussed _prefix operators_ ```(+) 5 4```, 
_infix operators_ ```5 + 4```, what about _postfix operators_?

Well, I'm going to go ahead and say it:
**there are no post-fix operators in Haskell**, regardless of there 
being a [postfix operators section on the GHC Manual](http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/syntax-extns.html#postfix-operators).

Why do I make such a bold claim then?
Because most people are used to postfix operators like these, from _C_/_C++_:

```c
x++
```

While these don't actually make sense in Haskell (immutable values, remember),
even a simpler (postfix) operator that returns a value plus one is not supported.

The best you have is (has the GHC Manual entry linked above states) just a very 
special case where a partially applied binary (function) operator can be used
in a postfix way, using the ```{-# LANGUAGE PostfixOperators #-}``` pragma.

So, this is valid:

```
{-# LANGUAGE PostfixOperators #-}
(5 +) -- Postfix operator, equivalent to
(+) 5 -- Prefix operator

(+5)  -- although in practice, this is what everyone would use
```

Do note that the parenthesis are required.

But this is not allowed:

<div class="alert alert-danger">

```Haskell
{-# LANGUAGE PostfixOperators #-}
let (+~+) = (+1)
	y = 3
	x = y +~+ -- postfix application invalid, its not a binary function
```
</div>

So, while I might have overreacted at start, in practice your are only allowed to
use postfix operators with partially applied binary operators, a use case where
you normally use prefix operators.

So, this is mainly useless IMHO (and non-idiomatic Haskell), but feel free to use it :)

### Basics (Part IX) | Lambdas

So far we only dealt with named functions with all parameters fully specified.
Haskell is one of several languages to offer anonymous functions.

Well then... what is an anonymous function, and what is it good for?

An anonymous function is a function definition that does not require a name (thus the anonymous part, duh),
and its mostly useful anywhere you need a function (namely other function parameters that expect a function)
but you don't feel the need to properly define a separate function, because you will not use this (anonymous)
function again.

But the current syntax does not allow for such a function to be defined, so we need a bit of extra syntax:

```Haskell
foo x y = x *y

-- is the same as

foo x= \y -> x*y

-- and is the same as
foo = \x -> \y -> x*y
```

And there you go... ```\x -> \y -> x * y``` is an anonymous function, that takes two parameters.

For comodity sake, you can simplify this to ```\x y -> x * y```, it is the same thing.
But, as expected, ```\(x,y) -> x * y``` is not. The first function expects two parameters,
the second expects a single
parameter, which happens to be a two element tuple (pair). 
It is easy to get confused if you're comming from other programming language.

#### Example

Take the ```comparing``` function from ```Data.Ord```:

```Haskell
comparing :: Ord a => (b -> a) -> b -> b -> Ordering
```

It accepts a function that transforms a type ```b```, 
possibly even without an ```Ord``` instance, into another
type ```a```, which in turn is suitable for comparison.
Then it also accepts two ```b```'s, and it returns the result
of their comparison, once they've been transformed into ```a```.

This is excelent for comparing complex records based on
only one of its fields, for example:

```Haskell

```




Data.Function
-------------

### [id](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Function.html#v:id)


The simplest of functions: **identity**.
Usefull whenever a given function expects another function as one of its parameters,
but you want to preserve the value being processes.

```Haskell
-- while this is not the actual definition in Data.Maybe, it is equivalent
fromMaybe :: a -> Maybe a -> a
fromMaybe default maybeValue = maybe default id maybeValue
```

<div class="alert alert-warning">

**Note:** When working with databases, you might be tempted to define a variable named ```id```.
And then use ```id``` later in the same function, and wonder why the compiler is
giving you strange compilation messages.  While a tempting name to use (and GHC 
won't does not consider an error to redefine it,
just a warning), **do not use variables named id**, for your own good!

</div>


### [const](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Function.html#v:const)

Sometimes you might want to use a given function that takes another function as a 
parameter, but you don't really want to define a new function, just return a constant
value. You could just use a lambda:

```Haskell
-- Consider the maybe function, with the following signature
-- maybe	:: b -> (a -> b) -> Maybe a -> b
-- So, use a default value if the maybe is Nothing, or use a function to operate in
-- the Just inner value, if the value is a Just.

let x = maybe 0 (\_ -> 1) maybeValue
```

The function defined above will ignore the Just value, and always return 1 for 
```Just``` values. While there is nothing wrong with the above definition, it is 
considered somehow more idiomatic to use instead:


<div class="alert alert-success">
```Haskell
let x = maybe 0 (const 1) maybeValue
```

</div>

Thus, the definition of ```const``` is pretty trivial:

```Haskell
-- | Constant function.
const                   :: a -> b -> a
const x _               =  x
```

### (.) (http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Function.html#v:.)

Function composition. Where to start.

Fun
Deceitively simple, in Haskell i


flip
----

($)
---

fix
---

on
--

### Further References

[More information from the oficial source](http://www.haskell.org/tutorial/functions.html#sect4.4.2)





