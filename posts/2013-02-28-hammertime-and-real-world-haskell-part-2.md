---
title: Hammertime and real world haskell, part 2
author: Clement Delafargue
tags: fp, haskell, hammertime
---

After a few months of complete inactivity on Hammertime, I've finally found
the time to get back on it. These last weeks, I have also played *a lot* with
Hakyll. Writing some Haskell gave me the material needed for the part 2 of my
"Real World Haskell" series. Hence this post.

## Build environment. Again.

### Cabal layout
As I said in the previous post, I had some trouble finding the good source
code layout, especially with respect to tests integration. Well, I've finally
managed to do that, but I wish there were better guidelines. As of now, there
is no "right way" of structuring code in a cabal setup. On one hand it
provides flexibility. On another hand it's a bit difficult for newcomers to
decide how to structure their files.

Some people in the Haskell community think default project layout could be
useful too (see <http://www.yesodweb.com/blog/2012/09/project-templates>)

I took inspiration from the [Lens package](https://github.com/ekmett/lens) and
asked for advice on `#haskell`. Now Hammertime has a reasonably clean layout.

### Tests

It was on the top of my todo list, so I took the time to integrate tests in
the build environment.

To integrate tests in cabal, the most simple way is to make it run an
executable which executes tests. If this executable returns `EXIT_SUCCESS`,
then cabal knows the tests passed.

While this can seem a bit tedious, this is quite well handled by the tools.

I used [TestFramework](http://hackage.haskell.org/package/test-framework) to
integrate tests. It allows to easily generate the test executable, which is
very convenient. Another nice feature is that it can group HUnit tests and
Quickcheck properties in tests suites.

I've not written Quickcheck properties yet, only HUnit tests. HUnit is on par
with conventional Unit testing frameworks. The only annoying thing is that you
have to manually add the tests to the test suite.

I've not used Quickcheck a lot for now and I think it will deserve its own
blog post.

### Continuous Integration

Once hammertime had its brand new test suite, the first thing I did was to
setup CI. I've been impressed by the ease of use of [Travis
CI](http://travis-ci.org). Travis was setup in roughly 10 minutes and its
github integration is amazing.

I also have to try [Drone.IO](http://drone.io) which seems promising and is
backed by a very reactive team. I asked them about haskell support Tuesday
evening and on Wednesday morning it was in production.

For small open-source projects, CI is free, so it'd be a shame not to use it.

## Thought process

Working with Hakyll was really interesting. It gave me the confirmation that
the best way to discover a new library is via its types. Thinking about types
upfront allowed me to solve problems with a very limited cognitive load.
Thinking only about types allowed me to combine lots of unknown bricks
whithout forcing me to think about how they worked. When working with a new
codebase, it's priceless.

I tend to only reason about types. It allows me to quickly combine functions
from the standard lib (with help from Hakyll), and care about writing code
only at the end of the process. When working with Scala and JS, I often resort
to reading the libraries' source code to understand how to use them. With
haskell, types are sufficient.

Regular unit tests are still very useful but not as important as in languages
with weaker type systems. Property-based tests with Quickcheck are really
interesting and deserve a dedicated article, but I'm not yet comfortable
enough with it.

## Wrap up

Writing "Real World" haskell (as opposed to playing with one-liners for code
challenges) is really interesting and helps me a lot to get more effective
with Haskell. It may not be as fun as writing high-level, ultra-generic code
with advanced concepts but it puts haskell on the same ground as the other
languages and shows its productivity boost in real-life situations.

## Hammertime

I've been able to put some work in Hammertime. It now has and a clean code
layout and unit tests, a working CLI parser and provides simple reports. I'm
working on turning `-Wall` on during the compiling phase. There are just a few
warnings left.
