---
title: Monoids, Newtypes and the Evil Human Resources Dept.
tags: haskell, monoid, newtype
---

_tl;dr:_ what are those newtypes on monoid good for

### The problem with Sum

When I read the excelent [LYAH](http://learnyouahaskell.com/),
namely the [monoid](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)
section, I must admit at first I didn't quite get the usefulness of newtypes like 
[Sum](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#t:Sum), 
[Product](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#t:Product)
or [First](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html#t:Sum).

I mean, let's take ```Sum```. You have a list of ```Int``` that you want to sum. To use this,
you must first convert it to the newtype ```Sum```

```Haskell
import Data.Monoid

let x = [1,2,3,4,5] :: [Int]

let y = map Sum x
```
And then apply ```getSum``` to extract the result:

```Haskell
let result = getSum . mconcat $ y

print result

> 15
```

It honestly seems dumb for something that can more easily be achieved with:

```Haskell
let result = foldr1 (+) x

```

Or, even simpler:

```Haskell
let result = sum x

```

So, what's the point about those Newtypes anyway? Aren't they more trouble than what their worth?

### Monoid records

My [eureka moment](https://en.wikipedia.org/wiki/Eureka_effect) happened in two parts:

* First, I was looking at the [semigroups](http://hackage.haskell.org/package/semigroups),
  a [simpler](http://fundeps.com/posts/haskell/2014-03-05-from-semigroup-to-bind/) version of monoids, and their definition as
  *anything that associates* really clicked.

* And then, I discovered [Generics.Deriving.Monoid](http://hackage.haskell.org/package/generic-deriving-1.6.2/docs/Generics-Deriving-Monoid.html)

So, if you have a long list of records that you want to combine, you can 'encode' how they should be combined in each of
the fields using a ```Monoid``` newtype. And then, you can automatically derive a monoid instance for the record using generics.

Let's see an example:

### Evil Director of Human Resources

_warning: dark humor ahead_

Let's say you are an [evil director of HR](https://en.wikipedia.org/wiki/Catbert) tasked with choosing a person to assign to
a project your company is bidding on. The project has some requirements:

* The resource must be senior
* The resource must have worked in at least three projects before
* The resource must have worked with Haskell

However, you are not sure if any of your employees fits the profile, as you only hire cheap labor. What should you do?

Easy! You combine each of the employees experience into a single super-profile!

Let start by defining each employee profile:

```Haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Generics.Deriving.Monoid

data EmployeeProfile = EmployeeProfile
  { age         :: First Int -- for legal reasons, you don't store the age of some employees
  , numProjects :: Sum Int   -- you want to gather as much experience as possible
  , haskell     :: Any       -- implict boolean
  , python      :: Any       -- implicit boolean
  } deriving (Eq,Ord,Show,Generic)
```

Now, lets insert our three employees:

```Haskell
empl0 = EmployeeProfile (First Nothing)  (Sum 1) (Any False) (Any True)
empl1 = EmployeeProfile (First Nothing)  (Sum 2) (Any True ) (Any True)
empl2 = EmployeeProfile (First (Just 25))(Sum 0) (Any False) (Any False)
```

So, not a great pool of workers. Let's combine them into a super worker!

```Haskell
-- Generics.Deriving.Monoid magic
instance Monoid EmployeeProfile where
   mempty  = memptydefault
   mappend = mappenddefault

let greatEmployee = mconcat [empl0,empl1,empl2]

print greatEmployee 

> EmployeeProfile { age = First {getFirst = Just 25}
				  , numProjects = Sum {getSum = 3}
				  , haskell = Any {getAny = True}
				  , python = Any {getAny = True}}
```

That's better! Now you've got an employee with 25 years of age, that has worked in three
projects and has Haskell experience!

Now imagine what you can do with an enormous pool of mostly unskilled labour! 
You can almost simulate a very competent Haskell programmer!

This was still a small example, with some added boilerplate.
The main catch here is that this boilerplate is always the same independently of the record size, as long as
all its fields are monoids themselves. 

Some of the fields could benefit from being a semigroup instead of monoid. For example, ```age``` should instead be a 
[Max](http://hackage.haskell.org/package/semigroups-0.12.2/docs/Data-Semigroup.html#t:Max), so that we would select
the eldest of ages, and thus be more compliant with the seniority requirement.
This is still not possible, since Semigroups do not support generic instances, but let's see if there is 
[openess to include it](https://github.com/ekmett/semigroups/issues/26).

To extract values from this structure, you also have some additional boilerplate, namely the newtype 'getter':

```Haskell
getAge :: EmployeeProfile -> Int
getAge = getFirst . age

hasHaskellExperience :: EmployeeProfile -> Bool
hasHaskellExperience = getAny . haskell
```

If you are bothered by having to memorize all those 'getters' and 'setters' (and are not using something more advanced like
[lenses](http://hackage.haskell.org/package/lens)), you can benefit from Darius Jahandarie package Newtype, that I have modified
to include generics support (shameless auto-promotion): [newtype-generics](http://hackage.haskell.org/package/newtype-generics)

Then, you just have to modify the code to:

```Haskell
import Control.Newtype

(...)

getAge :: EmployeeProfile -> Int
getAge = unpack . age

hasHaskellExperience :: EmployeeProfile -> Bool
hasHaskellExperience = unpack . haskell

```

Of course, It would be even nicer to automatically extract all the newtypes into a new structure, but as that would 
give rise to an (undefined / anonymous) new type, something that 
[it is not possible without template haskell](https://stackoverflow.com/questions/22299840/derive-a-record-datatype-without-template-haskell).

I hope you've liked this article. I have not bothered with the details of how generics work, but instead tried to give a more
hands on approach of why I found monoid structures useful.
Also, please note that I do not condone _evil human resources departments_ :)

### Addendum

As [Edward Kmett points out](http://www.reddit.com/r/haskell/comments/207yjp/monoids_newtypes_and_the_evil_human_resources_dept/cg0p7gt)
this practice is not without its problems. You are indeed forcing a monoid structure in your record that may not fullfil all
needs.

As he points out, lets suppose you indeed need to find a employee that fullfils all requisites, instead of 'inventing' one.
Then, you need an intersection.

You would then define a simpler structure:

```Haskell
data EmployeeProfile = EmployeeProfile
  { age         :: Maybe Int -- for legal reasons, you don't store the age of some employees
  , numProjects :: Int   -- you want to gather as much experience as possible
  , haskell     :: Bool  
  , python      :: Bool 
  } deriving (Eq,Ord,Show,Generic)

```

and then you could just have this:

```Haskell
print $ foldMap (\e -> (First (age e), Sum (numProjects e), All (haskell e))) [empl0,empl1,empl2]
```

That would return a tuple with the same information, without having to even declare ```EmployeeProfile``` as a Monoid,
hence forcing a given monoid structure on those fields.

Thus, it is, as usual, a matter of what you intend to do with your data.
If you have a long record, with the possibility of adding additional fields later, but you intend to combine them always
in the same manner, the solution presented in this post has some clear advantages, since you don't have to modify the monoid
instance manually for each field.

But if you can see yourself needing to extract different information using different monoids, then perhaps you need the simpler
data record, and instead you'll use the ```Monoid``` ```newtypes``` on your functions, rather than your data structure.

It's up to you, really :)

