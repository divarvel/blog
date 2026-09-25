---
title: It’s traverse!
author: Clement Delafargue
tags: haskell, traverse
---

_This is adapted from [a talk](https://cltdl.fr/talks/it-s-traverse.html) I gave
a couple times._

While _everybody_ should know that the answer is `traverse`, I’m not sure people
fully understand how often it actually is `traverse`, and why.

But mostly, I want to brag about one line of code. It
comes from [biscuit-haskell](https://github.com/eclipse-biscuit) and it looks
like this (type annotations added for clarity):

```haskell
type Scoped a = (Set Natural, a)

getCombinations :: [[Scoped Bindings]] -> [Scoped [Bindings]]
getCombinations = getCompose . traverse Compose
```

First, let me back up and introduce a few things that will help showcase `traverse`.
The running example will be reading secrets from environment variables. 

```haskell
-- this should be a newtype in an actual codebase, but let’s skip the noise
type ServiceName = String
type Secret = String

-- given "galactus", reads a secret from "GALACTUS_SECRET"
getSecret :: ServiceName -> IO Secret
getSecret name =
    let varName = fmap toUpper name <> "_SECRET" 
     in getEnv varName
```

Let’s start simple, and try to go from `[ServiceName]` to `[Secret]`. Since
reading secrets requires doing some `IO`, we’ll actually get `IO [Secret]`.

Looking at `traverse`’s type,
`Traversable t, Applicative f => (a -> f b) -> t a -> f (t b)`, and plugging `[]`
as `t`, `IO` as `f`, `ServiceName` as `a` and `Secret` as `b`, we get
`(ServiceName -> IO Secret) -> [ServiceName] -> IO [Secret]`. Then
`traverse getSecret` is exactly what we need.

If you’re familiar with Javascript, that looks like `Promise.all()`: iterating
on an array with an async operation, while still collecting a list of results.

The first reason why `traverse` is so versatile is that it works for any `Traversable`,
including many types from the standard library `[]`, `Map k`, `Either a`, `(,) a`.
This is already impressive compared to `Promise.all()` which only takes arrays.

With typeclasses, you can implement `Traversable` on your own types. And in
most cases, GHC can do it for you, thanks to `DeriveTraversable`:

```haskell
{-# LANGUAGE DeriveTraversable #-}

data RoseTree a = MkRoseTree a [RoseTree a]
  deriving (Functor, Traversable)

data Services a = MkServices
  { galactus :: a
  , magicbaby :: a
  }
  deriving (Functor, Traversable)
```

From all this, we can already see `traverse` exposes effectful iteration, while
being completely generalized on the datatype being iterated on.

Now, remember `traverse`’s signature. Not only does it work with any
`Traversable t`, but also with any `Applicative f`. We started with `IO`, of
course, but there are many types that are `Applicative`: `Maybe`, `Either`,
`(->) r`, `Monoid a => (,) a`, and of course all your favourite effect stacks
and monad transformers. With free applicatives, you can even conjure an
`Applicative` from thin air. Even if there is no `DeriveApplicative`, there are
_a lot_ of applicatives out there.

Even more interesting are instances of `Applicative` that are _not_ instances of
`Monad`. The most famous one is surely `Validation e`, which is able to
accumulate errors. A personal favourite is [`EnvVarParser`](https://blog.clement.delafargue.name/posts/2020-03-20-environment-variables-parsing-for-free-applicatives.html)
which describes a parser for environment variables, which, thanks to being just
`Applicative` and not `Monad`, able to describe expected variables and so on.
Same goes for `Parser`, from `optparse-applicative`. With this, you’re able to
build sophisticated, inspectable parsers out of arbitrary data structures. 

So `traverse` generalizes the concept of iteration on two axes:

- the data structure itself
- the kind of effect being performed

## Wait there’s one more thing

Arbitrary composition of `Traversable`s are also `Traversable`. Arbitrary
composition of `Applicative`s are also `Applicative` (crucially, this is not true
for `Monad`). This is witnessed by the `Compose` type.

If you want to `traverse` a `Services (Map String) a` with a function returning
`IO (Validation (NonEmpty Error))`, you only have to sprinkle a few `Compose` and
that’s it.

We already have something that generalizes the `for` loop, arguably one of the
most foundational programming concepts, on two orthogonal axes, and both of those
axes compose freely.

And that’s why it’s always `traverse`.
