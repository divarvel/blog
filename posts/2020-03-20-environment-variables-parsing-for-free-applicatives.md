---
title: Environment variables parsing for free (applicatives)
author: Clement Delafargue
tags:
---

As a _cloud native_™ citizen, I've been heavily
inspired by [the twelve factor app](https://12factor.net/).
One key point is application configuration through environment variables.

As the number of parameters grows, having a structured way to parse and validate them is 
paramount. In haskell, `base` provides a really bare-bones way to get them, with `getEnv` and `lookupEnv`. This gets cumbersome quickly.

```haskell
-- todo example of tedious env reading
```

There are a few libraries in haskell that allow to do that in a structured way, and you should
definitely use them instead of rolling your own. Anyway my goal is to trick you into learning
free applicatives, so bear with me.

## What we want

- parse (multiple) environment variables into structured values
- accumulate errors as needed (instead of crashing at the first error)
- inspect the parser to generate help
- allow easy composition / modification of parsers

Given this constraints, we already know that the parsers will have to be
_applicative_ and not _monadic_ (error accumulation, parser inspection)

```haskell
-- todo example of what we want
```

Now let's add a final constraint: we don't want to put too much work in it:
the interesting thing to talk about is how to parse _one_ environment variable:
what's its name, how to turn the string into another type, etc.
Composing multiple variables is not really new: we want to try every variable, 
collect and structure the results is everything is okay. If there are issues, collect
errors and fail.

## What are free applicatives anyway?

So, what we want to do is to describe one effect, and then have composition for free.
This sounds dangerously like free applicatives!

Explaining free applicatives in detail is way out of my league, but we can see what we can do with them:

```haskell
data Ap f a where
  -- omitted for sanity

-- functor instance
-- for *any* `f`. No constraints! Amazing!
fmap :: (a -> b) -> Ap f a -> Ap f g
-- applicative instance
-- for *any* `f`. No constraints! Amazing!
pure :: a -> Ap f a
(<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b

runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m
liftAp :: f a -> Ap f a
hoistAp :: (forall a. f a -> g a) -> Ap f b -> Ap g b 
```

todo explain quickly what free constructs are

## Back to work

### Creating basic parsers

Okay, so creating basic parsers are covered by the `Functor` and `Applicative` instances.

```haskell
-- todo examples
```

Now before going further, we need to think a bit more about this pesky `f`.

```haskell
-- todo show the underlying functor works
```

Ok, now that we got that out of the way, on to more complex things:

### Group modifiers

Applying group modifiers on parsers can be handled by `hoistAp` if you squint just right
(if it looks like `hoistServer` from servant, that's because it's quite close!

In our case, we don't want to change the `f` into `g`, we just want to modify `f` values

```haskell
-- todo prefix example with `hoistAp`
```

### Actually parsing the result 

Now the interesting part: we want to get actual values. So we need to eliminate the `Ap`.

`runAp` seems interesting. We just have to find the suitable `g`. Remember when I said we wanted
to get a structured value or _all the errors_? There's a very good reason this sounds like the
perfect job for `Validation`: that's because it is.

Now you'll tell me "Clément, stop messing with me, we still need to perform `IO`". And you'd be
right. But we know exactly what kind of `IO` we need: read the environment variables. So if we
have all the env vars we need in scope, we can write our `EnvVarParser a -> Validation (NonEmpty Error) a`. One way to have all the env vars we need is to read all the env vars, but we'll see in a bit
we can be a bit less _heavy handed_.

```haskell
-- todo the actual parsing function
```

### Inspecting the parsers

One of our goals was to be able to inspect parsers without actually running them, for instance for
generating documentation.

One simple example would be to list the needed variables names. That would allow to read only what's
needed in the `IO` phase.

For that, we could just run `runAp` and extract the variable names from the errors. That's a convoluted
way to do what `runAp_` does.

Here, we're interested in a list of environment variable names, so the monoid we're interested in
is the good old list. Funnily enough, `[]` is the _free monoid_.

What's even funnier is that we want to go from an applicative to just its accumulated effects.
There is an applicative functor that does just that: `Const`. And that's how `runAp_` is implemented.

```haskell
-- todo `getVars` example
```

Once you have that, you can extract anything you want from your applicative: variable names,
documentation. One nice thing it allowed us to do is to generate [dhall](todo) types from
the environment requirements. This allows to type-check the environment generation in the CI
pipelines directly, instead of waiting for them to fail during deployment _like cavepeople_.

Yay types!

## But I want to do more!

Optional values are rather easy on the parsing side, a bit harder on the metadata side.

Let's discuss two use cases that _seem_ simple but are actually out of applicatives' reach

### Conditional parsers

Applications running in production require more configuration that on local installations.
We could make everything optional, but that's not really satisfying. A common solution
is to start by looking up `ENVIRONMENT_NAME` and then decide what to do based on that.

The keen reader will notice that it looks like monadic behaviour. This can't be done purely (pun intended)
with applicative. A solution is to interleave monadic and applicative layers:
start by reading what you need, then decide on the actual parser based on that. You keep applicative
behaviour in the two layers, instead of making everything monadic and opaque.

That's what we have done. Another fancy solution would be to use [selective parsers](todo) instead.
Here we know all the possible paths, so we don't need the full monadic power.

### Optional parsers

The last one is trickier. Optional _variables_ are good, but what about optional _groups_?
This looks easy: `EnvParser a -> EnvParser (Maybe a)`. The common cases are easy enough:
if everything is defined, then return a `Just`, if nothing is defined `Nothing`, if there are
errors collect them. The last possibility is harder though: if some variables are defined but not
all, we want to warn the user, instead of silently ignoring input. And that's where this simple
requirement introduces a constraint that's not compatible with applicatives: variables are
interdependent. This could also be a job for selective functors. I'm not entirely sure.


todo wrap up
