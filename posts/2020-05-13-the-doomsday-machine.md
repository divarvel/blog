---
title: The doomsday machine
author: Clement Delafargue
tags: haskell
canonical: https://tech.fretlink.com/the-doomsday-machine/
---

![The russian ambassador in Dr Strangelove](/files/russian-ambassador-1.jpg)

Ah, the `do`\-notation. Is it good, is it
[bad](https://mail.haskell.org/pipermail/haskell-cafe/2007-August/030178.html);
who knows? It's good for beginners,
it's bad for beginners. It's [considered
harmful](https://wiki.haskell.org/Do_notation_considered_harmful).

A lot of [virtual
ink](https://joyofhaskell.com/posts/2017-05-07-do-notation.html)
has been spilled on this subject ([and still
is](https://twitter.com/search?q=https%3A%2F%2Ftwitter.com%2FFunctorFact%2Fstatus%2F1255566213092184070&src=typed_query)).
Let me try to add a new perspective, informed by the use of Haskell in
a heterogeneous team, in a professional setting. Then we'll be able
to gleefully conclude that _it depends_. But rather than fretting
too much over the destination, let's try to enjoy the journey.

## What's in a `do`?

Let's start by the beginning. `do` blocks are a syntactic construct
in the language Haskell. It provides an alternative syntax over the
use of `>>=`:

```haskell
-- without do
main :: IO ()
main =
  getLine >>= \name ->
  let greeting = "Hello " <> name
  in putStrLn greeting

-- with do
main :: IO ()
main = do
  name <- getLine
  let greeting = "Hello " <> name
  putStrLn greeting
```

That's the gist of `do`\-notation. There are a few additional features,
but we don't need to talk about them for now. The example is formatted
in a way that makes the correspondence obvious. You might be tempted
to think that `do` is a smart trick to remove syntactic noise when
writing monadic expressions. And you'd be right.

## Ten things I hate about `do`

![](/files/10-things-1.gif)

> Julia Stiles as Kat Stratford, saying "But mostly, I hate the way I don't hate you, not even close" in Ten Things I Hate About You

If you look at things this way, it's easy to discard `do` notation:
it doesn't add any expressive power, and it makes things more complex
for no good reason. As Paul Hudack nicely put it,

> Functions are in my comfort zone; syntax that hides them takes me out of my comfort zone.

Another issue is that it makes imperative code _too_ nice-looking,
thus earning Haskell its famous _finest imperative language_ badge
of shame. The familiarity provided by `do` notation might encourage
imperative-minded programmers into putting too many things in `IO`
(or any other monadic context, for that matter) instead of _composing
functions_ as `${DEITY:-or lack thereof}` intended.

## Love me `do`

![](/files/abbey-road.jpg)

> The Beatles, crossing the street from right to left

All that is good and well, but let's go back to our nice (and artificial) example:

```haskell
main :: IO ()
main =
  getLine >>= \name ->
  let greeting = "Hello " <> name
  in putStrLn greeting
```

What do you think happens when you insist on using function composition
in a team of Haskell programmers?

Yeah, that's right, `getLine >>= putStrLn . ("Hello " <> )`. And _in
real life_, stuff like `(runService =<< parseConfig) *> otherTask`
will pop up, because _composing functions is good, obviously_. I do
_love_ the smell of η-reduction in the morning, but hear me out:
what if (bear with me) η-reduction was not always good? Sometimes
you care about the pipeline of functions, sometimes you care about
the intermediate values.

## Why not both?

Now for my main thesis: using `do` over `bind` (`=<<` or `>>=`)
is _contextual_. And the context can be _very small_. Ask yourself:
what part of your program is important enough to be obvious when you
read it? Are all those intermediate variables bringing meaning?

```haskell
parseString :: String -> Handler Int
parseString = …

getString :: Handler String
getString = …
    
myDoHandler :: Handler Response
myDoHandler = do
  rawString <- getString
  value <- parseString rawString
  log $ "Got value " <> show value
  pure $ mkResponse value

myMixedHandler :: Handler Response
myMixedHandler = do
  value <- parseString =<< getString
  log $ "Got value " <> show value
  pure $ mkResponse value
  
myPointfreeHandler :: Handler Response
myPointfreeHandler =
 let logAnd = log . ("Got value" <>) . show >>= ($>)
 in mkResponse <$> getString >>= parseString >>= logAnd
```

Which one do you prefer? My favourite is `myMixedHandler`: I care about
`value`, so that makes sense to give it a name. On the other hand,
the intermediate `String` value is not that interesting, so I'm okay
with hiding it.

If your favourite is `myPointfreeHandler`, I don't know what to
say. Maybe you won't agree with my conclusions.

In my opinion, `do` and `bind` are _complementary_: `do` nudges
me into naming variables by providing a clean syntax. `bind` makes
function pipelines more obvious by hiding intermediary values. Both
have their uses, even on the same line!

## But `do` discourages applicative style!

That's right, `do` works with monads (since it desugars to `>>=`),
so that encourages a monadic style where applicative functors could
be enough. Two things:

- If you work with a type where applicative and monadic behaviour
  are inconsistent, you're in for nasty surprises and `do` blocks are
  not your most pressing issue
- Choosing between putting emphasis on values or functions is not
  inherently monadic

```haskell
myParser :: Parser (Int, Maybe String)
myParser = do
  char '@'
  intValue <- int
  many space
  stringValue <- optional string
  pure (intValue, stringValue)
  
myApplicativeParser :: Parser (Int, Maybe String)
myApplicativeParser =
  liftA2 (,)
    (char '@' *> int <* many space)
    (optional string)
```

Both parsers parse the same thing, but the second is _obviously better_
because it truly showcases the applicative nature of what we're doing.

Here, parser has both a `Monad` and an `Applicative` instance, so
that's just style.

What about a parser that is _not_ a `Monad`?

Let's put our [environment variables
parsers](https://tech.fretlink.com/environment-variables-parsing-for-free-applicatives/)
to work.

```haskell
data MyConfig
  = MyConfig
  { value2 :: String
  , value1 :: String
  , value3 :: Int
  }
  
configParser :: EnvParser MyConfig
configParser = MyConfig
  <$> required (str "VALUE_1")
  <*> required (str "VALUE_2")
  <*> required (int "VALUE_3")
```
    
Ah that's where applicative syntax shines! It's clear
and easy to read. Oh by the way did you spot the _surprising
behaviour_? Yes? Congrats then. Something really close happened to us,
got past code review, went to production and caused an outage. Yes we
were using newtypes instead of plain strings. No it did not help us
(`IsString` can be tricky). If you did not notice the issue, `VALUE_1`
maps to `value2`, and `VALUE_2` maps to `value1`.

The issue here is that we rely on the order of arguments in the
constructor. A nice rule of thumb when constructing records is to
make fields explicit:

```haskell
configParser :: EnvParser MyConfig
configPaser =
  let mkConfig value1 value2 value3 =
    MyConfig { value1 = value1
             , value2 = value2
             , value3 = value3
             }
   in mkConfig <$> required (str "VALUE_1")
               <*> required (str "VALUE_2")
               <*> required (int "VALUE_3")
```

That's a bit better, because everything is in the same function:
we don't have to look at the record definition to know what will
happen. Still, the names are one level away from the parsers.

It turns out that we can have `do` notation even without `Monad`,
thanks to `ApplicativeDo` (I've also thrown a `RecordWildCards`
in the mix since we're playing with _forbidden_ extensions.

```haskell
{-# LANGUAGE ApplicativeDo   #-}
{-# LaNgUaGe RecordWildCards #-}

configParser :: EnvParser Config
configParser = do
  value1 <- required (str "VALUE_1")
  value2 <- required (str "VALUE_2")
  value3 <- required (int "VALUE_3")
  pure Config{..}
```

Is it theoretically pure and syntactically minimal? No. Does it
provide easier to read code, even in messy, real-worldy context? I
do think so, yes.

### `ApplicativeDo` pitfalls

So like me, you think `ApplicativeDo` is criminally
underused? Good! Now might be a good time to mention two things:

`ApplicativeDo` changes the behaviour of all `do` blocks. So if you
are using types with inconsistent `Applicative` and `Monad` instances,
you may curse a bit. Purescript solved it nicely by introducing `ado`,
to make the behaviour explicit. Personally, I try to only enable
`ApplicativeDo` in small modules where the only `do` blocks are over
types with no `Monad` instance.

Another pitfall is a somewhat surprising behaviour with `let` bindings
in `do` blocks:

```haskell
-- this requires a `Monad` instance on `EnvParser`
myParser :: EnvParser Result
myParser = do
  v1 <- v1Parser
  v2 <- v2Parser
  let v3 = f v1 v2
  pure $ Result v1 v2 v3
```

The way `do` blocks are de-sugared leads to the use of `>>=` when
there are `let` bindings.

You'll run in a similar issue if you try to pattern-match on the
left-hand side of `<-`:

```haskell
-- this requires a `Monad` instance on `EnvParser`
myParser :: EnvParser Result
myParser = do
  (v1, v2) <- bothVsParser
  -- you can bypass the limitation with a lazy pattern
  -- ~(v1, v2) <- bothVsParser
  v3       <- v3Parser
  pure $ Result v1 v2 v3
```

Thankfully there is a simple solution for both issues: using a `let
… in` expression as the last part (after `pure`). This is always
possible for applicative composition.

```haskell
myParser :: EnvParser Result
myParser = do
  v1 <- v1Parser
  v2 <- v2Parser
  pure $ let v3 = f v1 v2
          in Result v1 v2 v3
          
myOtherParser :: EnvParser Result
myOtherParser = do
  both <- bothVsParser
  v3   <- v3Parser
  pure $ let (v1, v2) = both
         in Result v1 v2 v3
```

## What did we learn then?

![](/files/youtube-video-gif.gif)

> I guess we learned to not `do` it again

There are multiple layers to the `do` notation. At first you can
see it as a way to make Haskell look more imperative. It's a good
beginner-friendly crutch, but you soon grow past it: function
composition is what Haskell is all about, anyway. That's where I
was for a long time. But using Haskell in a professional setting
led me to a more nuanced opinion: the language provides different
tools, it's up to me to use them in order to get something that best
expresses my intent. Of course nothing is really perfect and `do`
notation has its issues, but it can be of great help when it comes
to choosing how to structure my code.

Haskell is a mature language and, perhaps a bit counter-intuitively,
not that opinionated: it provides developers with a lot of tools
and alternative ways to do the same thing. Personally, I love it
because I can craft code in a way that lets me outline what I want,
_depending on context_. Of course a pragmatic compromise like this is
not the best choice for everyone, some may prefer [a more opinionated
language](https://golang.org) that makes more decisions for you.
