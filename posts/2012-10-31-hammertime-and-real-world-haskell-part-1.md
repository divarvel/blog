---
title: Hammertime and real world haskell, part 1
author: Clément Delafargue
tags: fp, haskell
---

I've spent the last few evenings on a pet project,
[Hammertime](http://github.com/divarvel/hammertime). It's a **CLI
time-tracker** which allows me to collect data on how I use my time. I intend to
generate SVG reports later on.

For instance, before starting to read my news feeds:

    hammertime start personal news

Then, proceeding to read work-related emails:

    hammertime start work emails

For now, hammertime is just a helper to register events associated to a timestamp.
Its model is fully sequential (no parallel tasks). It uses a flat file to
store data.

Hammertime's feature set is arguably not the more important part of this
project.  Hammertime is my first "real" haskell project. By that I mean a
project that I will really use and which is inherently impure (to some extent
at least). Most of the haskell I write is usually based on one-liners typed in
``ghci``. With hammertime, I intend to bridge the gap between fun, purely
functional, highly abstract code (the kind of code I've never written outside
of haskell-land) and more concrete, everyday code, to see how Haskell's beauty
can help me in a more mundane task.

I intend to **track my work on hammertime on this blog** to write about **how
real world haskell development compares to other, mainstream workflows**.

##Development environment

###Build tool

Most of the time I just load a ``.hs`` file from ``ghci`` and I get running.
This time, I've put up a cabal setup, just to keep track of the dependencies
and to quickly provide a usable executable (even though I can't visualize
data, I've started to collect some). However, hammertime is still in one file,
so I've kept cabal from my development workflow. Also, I must admit that I'm a
bit scared of its dependency management, which is bad,
[to say the least](http://www.txt.io/t-2kv5h). Fortunately, I use cabal only
for development, not for installing software. My distro has a fairly good
support of haskell
(see [Exherbo's haskell repository](http://git.exherbo.org/summer/repositories/haskell/index.html)).

That said, having fiddled with snap and yesod, cabal has always been a PITA
with conflicting dependencies. I've tried ``cabal-dev`` but rebuilding
everything for every new install was a huge time sink. I somehow miss the java
world with ``sbt`` and ``maven`` which handle these kinds of problems quite
well.

There is no real equivalent of sbt's REPL (which starts with the right
classpath), but I use a ``.ghci`` file wich makes ghci execute statements when
starting.

My reference for a good cabal layout was [Tony Morris' tic-tac-toe
example](https://github.com/tonymorris/course-answers), until I found
(precisely when writing down this very sentence) [Tony Morris'
haskell-package]( https://github.com/tonymorris/haskell-package). This
skeleton package provides a very good starting point for a package.

I've not yet written unit tests for hammertime (which is bad, I know), and I
must admit I'm a bit put off by how they're integrated in cabal. Again, I miss
``sbt`` for this.

Cabal is able to provide all of that, but not out of the box.


###Playing with the code

I don't really like IDEs for lots of reasons (cf [IDEs are a language
smell](http://www.recursivity.com/blog/2012/10/28/ides-are-a-language-smell/))
and I've not used one for years. For a full-blown java dev, I would certainly
use one, but right now, I'm using languages expressive enough to avoid IDEs (I
even wrote a small java project from ``sbt`` the other day and played with it
via the ``REPL``, but that's another story).

Overall, I've been able to keep my habits: **``vim`` + ``ghci`` is a great combo**
to play with code. One nice side effect is that it gives a strong incentive to
write really short top-level methods, which are easier to use in ghci.

Playing with side-effecting code in ghci is quite simple too. For instance:

    Prelude Data.Time> :t getCurrentTime
    getCurrentTime :: IO UTCTime

    Prelude Data.Time> getCurrentTime
    2012-10-30 22:30:32.980922 UTC

    Prelude Data.Time> :t it
    it :: UTCTime

With the special ``it`` variable, I directly have access to the unwrapped
``UTCTime`` value after it has been evaluated. This makes ``ghci`` even more
useful when trying new code.

I use ``pointfree`` from time to time to refactor lambdas into a more elegant
style. I'm quite cautious with this tool because it can produce cryptic
formulations, but from time to time it's useful.

###Documentation

Once again, it's quite the same for haskell or scala or JS, I just browse the
reference websites when I want to have precise info on some part of the API.
Since I'm quite new to haskell, my knowledge of the standard library is a bit
thin, and that's where [Hoogle](http://www.haskell.org/hoogle) is a **huge**
time-saver. [Hayoo!](http://holumbus.fh-wedel.de/hayoo/hayoo.html) is more
useful for packages outside the standard lib, but for these I prefer to read
thoroughly the documentation. These tools allow me to **find functions based on
their type**. Since haskell's type system is really expressive and functions
are highly generic, most often the type describes exactly the feature.

Even though I almost know [LYAH](http://learnyouahaskell.com/) by heart and I
have a decent knowledge of common haskell constructs, **I don't know anything
about basic stuff like writing to a file or reading command line args**. That's
where [Real World Haskell](http://book.realworldhaskell.org/) is of tremendous
help. I've not read it completely yet because it frightened me a bit, but the
more I read it, the more I like the choices taken by its authors. They offer a
good vision of their process in end-to-end case studies.

I just wish they explained how to package Haskell software with cabal.

##Wrap up

Apart from the problems with cabal, developing in Haskell is a nice
experience. ``ghci`` and *hoogle* are making me insanely more productive when
I'm in need of documentation. Hoogle is one of the best proofs that a good
type system is essential for readability. I may not have been as productive
with new libraries as I would have with an IDE and some ``C^Space`` magic, but
I've learned more about the libraries I'm using.


Next part will be about the **thought process when prototyping haskell code**.
I've found it to be quite different from the one I adopt when writing scala or
JS.

The code is [available on github](http://github.com/divarvel/hammertime). Feel
free to browse it and give me feedback. As I said, it's my first haskell
project and I'm eager to improve.
