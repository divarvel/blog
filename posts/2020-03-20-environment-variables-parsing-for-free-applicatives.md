---
title: Environment variables parsing for free (applicatives)
author: Clement Delafargue
tags: haskell, free applicative
canonical: https://tech.fretlink.com/environment-variables-parsing-for-free-applicatives/
fretlink_post: true
---

_Edit 2020-05-04: I have been pointed to [envparse](https://hackage.haskell.org/package/envparse), which is touted as "optparse-applicative, but for enviroment variables" and is implemented with free applicative functors. Sounds familiar? It is more polished and has a better design than what's shown in the article, but it is strikingly similar to what I came up with._

As _cloud native_™ citizens, we have been heavily inspired by [the twelve factor app](https://12factor.net/). One of its key points is application configuration through environment variables.

As the number of parameters grows, having a structured way to parse and validate them is paramount. In haskell, [the `base` library](https://hackage.haskell.org/package/base) provides a really bare-bones way to get them, with [`getEnv`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:getEnv) and [`lookupEnv`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:lookupEnv). This gets cumbersome quickly, if you want _proper_ error reporting.

```haskell
module Env where

-- either
import Data.Either.Validation
-- base
import Data.List.NonEmpty
import System.Environment

data Config
  = Config
  { myValue :: String
  , myOtherValue :: String
  }

-- this is the easiest way, but it will crash at the first missing
-- environment variable: `getEnv` returns an `IO String`, so it
-- throws if the variable is not defined. There is no way to 
-- report all the errors.
-- This also does not parse the variable contents
readEnv :: IO Config
readEnv = Config
  <$> getEnv "MY_VALUE"
  <*> getEnv "MY_OTHER_VALUE"
 
-- Slightly better, returns all the missing environment variables:
-- `lookupEnv` returns an `IO (Maybe String)` so it does not throw
-- exceptions on missing values. Once we have run all the `IO` steps,
-- we can accumulate errors thanks to `Validation`
-- This still only handles `String`s, but `getValue` could handle
-- it as well, at the cost of a bit more complexity
betterReadEnv :: IO (Validation (NonEmpty String) Config)
betterReadEnv = do
  let getValue name = maybe (Failure $ pure name) Success <$> lookupEnv name
  myValue <- getValue "MY_VALUE"
  myOtherValue <- getValue "MY_OTHER_VALUE"
  pure $ Config <$> myValue <*> myOtherValue
```

There are a few libraries in haskell that allow to do that in a structured way (for instance [envy](https://hackage.haskell.org/package/envy)), and you should definitely use them instead of rolling your own. Anyway my goal is to trick you into learning free applicatives, so bear with me.

## What we want

- parse (multiple) environment variables into structured values
- accumulate errors as needed (instead of crashing at the first error)
- inspect the parser to generate help
- allow easy composition / modification of parsers

In a way, we want something like the amazing [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative), but for environment variables. Note that typeclasses are not mentionned here: that's on purpose. While they are common for serialization use cases, here we want to be as explicit as possible.

Given this constraints, we already know that the parsers will have to be  
_applicative_ and not _monadic_ (error accumulation, parser inspection). This is in line with the analogy with optparse-applicative.

```haskell
import MySuperLibrary

import Data.Text (Text)

data SubConfig
  = SubConfig
  { subItem :: Text
  }

data Config
  = Config
  { myValue          :: Text
  , someNumber       :: Integer
  , anOptionalThingy :: Maybe Text
  , subConfig        :: SubConfig
  }

-- We want to be able to inspect the parser, so we need an
-- actual value, not just a function. This also makes it easier
-- to compose parsers
-- To be able to accumulate errors, we need something that is
-- applicative _only_
configParser :: EnvParser Config
configParser =
  Config <$> required (textParser "MY_VALUE")
         <*> required (intParser "SOME_NUMBER")
         <*> optional (textParser "OPTIONAL_THINGY")
         <*> prefixed "SUB_" (SubConfig <$> required (textParser "ITEM"))

-- same as with optparse-applicative, the lib can display the
-- error itself and give us a convenient helper
readConfig :: IO Config
readConfig = readFromEnv configParser

-- we want to be able to generate documentation to describe all
-- the variables used by the application
configHelp :: Text
configHelp = renderHelp configParser
```

Now let's add a final constraint: _we don't want to put too much work in it_; the interesting thing to talk about is how to parse _one_ environment variable: what's its name, how to turn the string into another type, etc.  
Composing multiple variables is not really new: we want to try every variable, collect and structure the results if everything is okay. If there are issues, collect errors and fail.

[A complete implementation](https://gist.github.com/clementd-fretlink/3a54cc5ef80ae61b584edac79dff40b2) is available in a gist. It is a bit different from the examples of the blog post, which have been simplified.

## What are free applicatives anyway?

So, what we want to do is to describe one effect, and then have composition for free. This sounds dangerously like a free _something_!

Free constructions lets you turn a base structure into a more powerful one: for instance, the free monoid over any type is the list. Free monads are another example: they let you get a monad out of any functor. Usually free constructs let you _build_ a value with the desired property (introduction), and then it's up to you to _interpret_ them into what you want (elimination).

Here, we want to turn something describing how to parse a single variable into something that parses multiple variables, with applicative composition. Free applicatives provide us that. Funnily enough, one motivating example from the [free applicatives paper](https://arxiv.org/abs/1403.0749) is a parser for command-line interface arguments. A nice confirmation that "optparse-applicative, but for environment variables" was not too far off the mark.

The [`free`](https://hackage.haskell.org/package/free) package provides us with a host of free constructions, including [free applicatives](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html).

I don't feel confident explaining what free applicatives _are_ and _how_ they work, but here, having a look at _what_ they can do will be enough.

```haskell
-- module Control.Applicative.Free

-- a free applicative is a _data structure_
data Ap f a where
  -- we don't need to talk about the contructors here

-- It gives us a functor instance for *any* `f`.
-- No constraints! Amazing!
fmap :: (a -> b) -> Ap f a -> Ap f b
-- It gives us an applicative instance for *any* `f`.
-- No constraints! Amazing!
pure :: a -> Ap f a
(<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b

-- given a way to turn the initial `f` into an applicative `g`,
-- we can eliminate the `Ap` wrapper
runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a

-- we can also eliminate the `Ap` to get a `Monoid`
runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m

-- we can turn a `f` into an applicative
liftAp :: f a -> Ap f a

-- we can change the `f` into a `g` inside the `Ap`
hoistAp :: (forall a. f a -> g a) -> Ap f b -> Ap g b 
```

## Back to work

### Creating basic parsers

The first step is the actual work: how to describe a parser for a _single_ environment variable.

For simplicity, I won't handle optional values, only required ones. We need the variable name and a parser function.

```haskell
import Data.Either.Combinators (maybeToRight)
import Text.Read (readMaybe)

data EnvVarParser a
  = EnvVarParser
  { parser :: String -> Either String a -- the value or a parsing error 
  , name :: String
  } deriving Functor
 
textParser :: String -> EnvVarParser Text
textParser name = EnvVarParser (pure . pack) name

intParser :: String -> EnvVarParser Integer
intParser name =
  let parser = maybeToRight "not an integer" . readMaybe
  in EnvVarParser parser name
```

Once we have that, we can use `[liftAp](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html#v:liftAp)` to get an applicative. From there, we can create basic parsers thanks to its `Functor` and `Applicative` instances.

```haskell
type EnvParser = Ap EnvVarParser

required :: EnvVarParser a -> EnvParser a
required = liftAp

data SubConfig
  = SubConfig { subItem :: Text }

data Config
  = Config
  { textValue :: Text
  , intValue  :: Integer
  , subConfig :: SubConfig
  }
  
configParser :: EnvParser Config
configParser = Config
  <$> required (textParser "TEXT_VALUE")
  <*> required (intParser "INT_VALUE")
  <*> (SubConfig <$> required (textParser "SUB_ITEM"))
```

OK, now that we got that out of the way, on to more complex things:

### Group modifiers

Applying group modifiers on parsers can be handled by `[hoistAp](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html#v:hoistAp)` if you squint just right. Here we don't want to turn `EnvVarParser` into another type, we want to apply a modification on it. So `hoistAp`'s type becomes `(forall x. EnvVarParser x -> EnvVarParser x) -> (EnvParser a -> EnvParser a)`: it lifts a function modifying a parser for a _single_ environment variable into a function modifying the whole tree of parsers. Don't get intimidated by the `forall` bit: since our function will be called on each variable parser, we can't assume anything about the parsed types: we only care about modifying the metadata in a consistent way.

Here is how the `prefix` modifier is implemented. It allows to add a prefix to all the variable names, to avoid repetition or repurpose an existing parser.

```haskell
addPrefix :: String -> EnvVarParser a -> EnvVarParser a
addPrefix prefix parser = parser { name = prefix <> name parser }

prefixed :: String -> EnvParser a -> EnvParser a
prefixed prefix = hoistAp (addPrefix prefix)
```

Side note if you're familiar with [servant](https://hackage.haskell.org/package/servant): this is quite close to `[hoistServer](https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server.html#v:hoistServer)`: it lets us apply a transformation on all the handlers in an API tree. Look at the signatures, they're really close. The name `[hoist](https://hackage.haskell.org/package/mmorph-1.1.3/docs/Control-Monad-Morph.html#v:hoist)` comes from the `mmorph` package (it stands for "Monad Morphisms").

### Actually parsing the result

Now the interesting part: we want to get actual values. So we need to eliminate the `Ap`.

[`runAp`](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html#v:runAp) seems interesting. We just have to find the suitable `g`. Remember when I said we wanted to get either a structured value or _all the errors_? There's a very good reason this sounds like the perfect job for `Validation`: that's because it is the perfect job for `Validation`.

Now you'll tell me "Clément, stop messing with me, we still need to perform `IO`". And you'd be right. But we know exactly what kind of `IO` we need: read the environment variables. So if we have all the env vars we need in scope, we can write our `EnvVarParser a -> Validation (NonEmpty Error) a`. One way to have all the env vars we need is to read all the env vars, but we'll see in a bit that we can be a bit less _heavy handed_.

```haskell
data EnvError
  = Missing String
  -- ^ The name of the missing variable
  | ParsingError String Text
  -- ^ The name of the variable and the parsing error
  deriving Show

runEnvParser :: [(String, String)]
             -- ^ We need to close over the environment
             -> EnvVarParser a
             -> Validation (NonEmpty EnvError) a
runEnvParser env (EnvVarParser parser name) =
  case (lookup name env) of
    Nothing -> Failure . pure $ Missing name
    Just v  -> either (Failure . pure . ParsingError name) Success $ parser name
    
readFromEnv :: IO (EnvParser a)
readFromEnv = do
  env <- getEnvironment
  case runAp (runEnvParser env) of
    Success a -> pure a
    Failure errors -> fail (show errors) -- this can be done better
```

Oh by the way, if you look at `runEnvParser`, you will notice that it is completely decoupled from environment variables. So we can reuse not only _parsers_, but also _core machinery_, to read variables from other key-value stores, for instance hashicorp's vault. Another win for interpreters!

### Inspecting the parsers

One of our goals was to be able to inspect parsers without actually running them, for instance for generating documentation.

For example, we would like to list the needed variables names. That would let us read only what's needed in the `IO` phase.

For that, we could run `[runAp](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html#v:runAp)` and extract the variable names from the errors. That would be a convoluted way to do what `[runAp_](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html#v:runAp_)` does.

Here, we're interested in a list of environment variable names, so the monoid we're interested in is the good old list. So we are turning a _free applicative_ into a _free monoid_. That's cool!

What's even better is that we want to go from an applicative to just its accumulated effects. It turns out there is an applicative functor that does just that: `Const`. And that's how `runAp_` is [implemented](https://hackage.haskell.org/package/free-5.1.3/docs/src/Control.Applicative.Free.html#runAp_).

Anyway, back to listing environment variables names

```haskell
getVars :: EnvParser a -> [String]
getVars = runAp_ (pure . name)
```

Yes, that's it.

Once you have that, you can extract anything you want from your applicative: variable names, documentation. One nice thing it allowed us to do is to generate [dhall](https://dhall-lang.org) types from the environment requirements. This allows to type-check the environment generation in the CI pipelines directly, instead of waiting for them to fail during deployment _like cavepeople_.

Yay types!

## But I want to do more!

Earlier, I papered over `optional` support. It's not too complicated to implement, but it adds some complexity (and room for inconsistencies) in `EnvVarParser`. Alongside the blog post is a full example, if you want to see how it works.

Let's discuss instead two use cases that _seem_ simple but are actually out of applicatives' reach.

### Conditional parsers

Applications running in production require more configuration than on local installations. We could make everything optional, but that's not really satisfying. A common solution is to start by looking up `ENVIRONMENT_NAME` and then decide what to do based on that.

The keen reader will notice that it looks like monadic behaviour. This can't be done purely (pun intended) with applicative. A solution is to interleave monadic and applicative layers: start by reading what you need, then decide on the actual parser based on that. You keep applicative behaviour in the two layers, instead of making everything monadic and opaque.

That's what we have done. Another fancy solution would be to use [selective parsers](https://hackage.haskell.org/package/selective) instead. Selective parsers are quite recent middle ground between _applicative functors_ and _monads_: you want some effect to depend on a previous one, **but** you can in advance enumerate the possible paths. It's not fully arbitrary monadic power. The good news is that there are [free selective functors](https://hackage.haskell.org/package/selective-0.4/docs/Control-Selective-Free.html). I still have no idea on how to use them to solve this, but that would be an interesting discussion to have, so please ping me if you want to discuss it.

### Optional parsers

The last one is trickier. Optional _variables_ are good, but what about optional _groups_?  
This looks easy: `EnvParser a -> EnvParser (Maybe a)`. The common cases are not too complicated:

- if everything is defined, then return a `Just`;
- if nothing is defined `Nothing`;
- if there are errors collect them.

There is one last possibility that is harder, though:

- if some variables are defined but not all

In that case, we want to warn the user, instead of silently ignoring input. And that's where this simple requirement introduces a constraint that's not compatible with applicatives: variables are interdependent.

I've tried a few things there, the most promising is to use a free `Alternative` (after all, it provides that out of the box [`optional :: Alternative f => f a -> f (Maybe a)`](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:optional)), but I was not satisfied with anything, so I left it at that. It's always possible to emulate this behaviour with some boilerplate (declare all the variables as optional, and then add a post-processing pass after the actual parsing).

## Closing remarks

The goal of this article is to showcase the use of free applicatives on a concrete case. I'm not advocating the use of free applicatives everywhere, rather trying to show that they're convenient, and not too hard to use. From a _practical_ standpoint they're great as long as you need purely applicative behaviour: extended use cases get cumbersome quickly. For instance, optparse-applicative uses its own type and does not use free applicatives. At some point I guess that it's easier to extend things with your own type. From a _pedagogical_ standpoint, I _love_ how free applicatives showcase the essence of applicative functors, especially the `runAp` / `runAp_` pair. There is no way to cheat or to accidentally introduce a behaviour that's _too powerful_.

One last remark: there are several implementations of free applicatives. I have only tested the naive one, as I did not have any performance constraint. The three implementations ([`Control.Applicative.Free`](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html), [`Control.Applicative.Free.Fast`](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free-Fast.html) and [`Control.Applicative.Free.Final`](https://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free-Final.html)) all expose the same API, so it should be possible to try them all out with minimal fuss.

