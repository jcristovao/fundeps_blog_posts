---
title: Haskell Cheat Sheets (Part 1)
author: João Cristóvão
tags: haskell , cheatsheet, typeclass
---

_tl;dr:_ : see [this](/tables/FromSemigroupToMonads.pdf).

If you look for Haskell cheat sheets on the web, you get:

* [Code Slower 14 pages cheat sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf)
* [Haskell Wiki Reference Card](http://www.haskell.org/haskellwiki/Reference_card)
* [A short one with the basics](http://www.cheat-sheets.org/saved-copy/Haskell.Haskell_Cheat_Sheet.pdf)
* [And another one](https://wincent.com/wiki/Haskell_cheat_sheet)
* [Ultimate Haskell Cheat Sheet](https://github.com/rudymatela/ultimate-cheat-sheets)

Personally, I find none of them particular useful. While the last three are useful for the 
absolute beginner, it seems that there's a gap from there forward.

Of course, the usual answer is [why do you need a cheat sheet, if there is hoogle](https://stackoverflow.com/questions/1162360/haskell-function-cheat-sheet),
but while [hoogle](http://www.haskell.org/hoogle/) and [hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) are great,
they only help you when you know what you are looking for exactly, namely the type signature.

Sometimes, it is useful to just have a lot information condensed in such a way that's easy 
to analyse,
so that you can make informed decisions or just easily discover additional functions.

With that purpose in mind, I plan to launch some Haskell cheat sheets, with different levels
of complexity and applicability, but with some clear goals in mind:

* Succinct 
* Ready to print, for the paper lovers like me
* Frequently updated and thus, one expects, always correct

The first Haskell sheet cheat I'll be releasing is one were I illustrate:

* [Semigroup](http://hackage.haskell.org/package/semigroups)
 , [Monoid](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html)
 , [Alt](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Alt.html)
 , [Plus](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Plus.html)
 , [Apply](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Apply.html)
 , [Applicative](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Applicative.html#t:Applicative)
 , [Alternative](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Applicative.html#t:Alternative)
 , [Bind](http://hackage.haskell.org/package/semigroupoids-4.0/docs/Data-Functor-Bind.html)
 , [Monad](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html#t:Monad)
 , [MonadPlus](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html#t:MonadPlus) hierarchy
* Fundamental operations supported by each type class
* Semantics of the binary operation (Choice, Combination, Both, Neither)
* Type Class restrictions

![](/images/fromsemigroup.png)


__You can get it [here](/tables/FromSemigroupToMonads.pdf).__

__If you don't know half of those type classes, please see my 
[next post](http://fundeps.com/posts/haskell/2014-03-05-from-semigroup-to-bind),
for a quick and dirty introduction.__

These tables were made incrementally as I was learning each type class.
They are still incomplete (namely the second page is still a partial copy-paste of the first),
and they are - get ready to puke - in [LibreOffice format](https://github.com/jcristovao/haskell-cheatsheets),
not Latex. But for these really complex tables I find it more useful: but I'm open to suggestions.

There are probably some lingering errors, and the table were not, unfortunately,
made through automated code analysis. 
I would very much like that, but right now I see it as a very time consuming task...

Please feel free to identify mistakes, make suggestions or otherwise give your feedback on these.
The original file is [at GitHub](https://github.com/jcristovao/haskell-cheatsheets).

Thanks!


