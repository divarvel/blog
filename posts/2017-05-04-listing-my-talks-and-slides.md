---
title: Building a small homepage
author: Clement Delafargue
tags: haskell, clever-cloud
---

After a giving a talk, I'm regularly asked for a link pointing to the slide
deck. I used to publish an article on my blog, but that took too much time.
Instead, I've created a small web page which lists my talks, with videos and
slides. In the next post, I'll show how I'm using the physical web to broadcast
its URL while I'm in a conference.

You can have a look at the page <https://clementd.cleverapps.io> and at its
[source code on github](https://github.com/divarvel/physical-homepage).

# Choosing a web stack

Since it's a side project, I've decided to experiment with new tools. I've
played a bit with rust and `rocket.rs`, but it's not sufficiently mature for me
(it requires nightly, and templating is only available through mustache or
mustache-like languages). Now that Clever Cloud has [native haskell
support](https://www.clever-cloud.com/doc/haskell/haskell/), I decided to use
it. In the past, I've played with both snap and yesod, but for a simple
application like that, I chose to go with
[scotty](https://hackage.haskell.org/package/scotty). For templating, I've used
[blaze-html](https://jaspervdj.be/blaze/tutorial.html).

## Routing

Scotty provides a thin layer over [WAI](https://www.stackage.org/package/wai),
mainly routing, as well as a context wrapper (which I don't use).

```haskell
main = scotty 3000 do
  get "/" $
    html $ "Ohai"
```

## Templating

Over the years using Play Framework, I've come to love its type-safe templating
system,
[Twirl](https://www.playframework.com/documentation/2.5.x/ScalaTemplates).
Instead of viewing templates as chunks of HTML with placeholders, it lets you
describe templates as functions taking data as input and producing HTML as
output. It sidesteps all the common issues of template inheritance, helpers,
etc as it allows you to abstract your templates with simple functions. Of
course, since you choose the types of input data, your templates and their use
is checked at compile time.

My biggest gripe with Twirl is that it's an external DSL resembling HTML, but
with specific exceptions, that are hard to understand without a good
understanding of the underlying parser. In practice, when you stray from common
uses, you just end up adding or removing spaces and blank lines until it
compiles.

As always, Haskell-land already has the solution I've been dreaming for without
knowing it exists. In the case of templating, there are two embedded DSLs,
[blaze-html](https://jaspervdj.be/blaze/tutorial.html) and
[lucid](https://hackage.haskell.org/package/lucid#readme). I've used
blaze-html, but lucid seems to be even better. In both cases, basic functions
allow you to create html blocks.  HTML elements are monadic, so you can compose
them with `do` blocks. It's not the purest use of monads I know, but it allows
to declutter the resulting code.

```haskell
import qualified Text.Blaze.Html5            (toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

paragraph :: Text -> Text -> H.Html
paragraph title text = H.div ! A.class_ "wrapper" $ do
  H.h1 (toHtml title)
  H.p  (toHtml text)
```

Since I wanted a mobile-friendly page, I've used [Material Design
Lite](https://getmdl.io/) to get a material design look and feel. The HTML you
have to write however, is not really *lite*. That's where haskell's abstraction
capabilities come in handy.

## Stats gathering

Just for fun, I've decided to use EKG, haskells metrics gathering system. My
first try used EKG's HTTP interface for metrics exposition, but this pulled
`snap-core`, and it felt a bit… *too much*.  I've decided to push metrics
trough the *init* protocol instead.

Since `scotty` is based on WAI, I was able to use
[`wai-middleware-metrics`](https://hackage.haskell.org/package/wai-middleware-metrics),
to gather web-related metrics (hit count, responses count, latency
distribution, usw…).

```haskell
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import System.Metrics (newStore)
import System.Remote.Monitoring.Statsd (defaultStatsdOptions, forkStatsd)
import Web.Scotty

main :: IO ()
main = do
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  forkStatsd defaultStatsdOptions store
  scotty 3000 $ do
    middlemare $ metrics waiMetrics
    get "/" $
      html $ "ohai"
```

The cool thing is that these metrics are automatically collected and gathered
by Clever Cloud :-)

## Development environment

Both snap and yesod provide auto reload dev environment, but raw warp doesn't.
`stack run --file-watch` does auto compilation, but it kills and reloads the
server, which doesn't really provide a save-refresh cycle. Thankfully,
Guillaume Bort (from playframework's fame), whose taste for simplicity matches
mine, worked on dev loop. Directly inspired by `play run`, but not tied to any
framework, it wraps the compilation phase and exposes the app behind a small
reverse proxy. If compilation fails, it displays a nice error message in the
browser. Another nice thing is how it handles environment variables: it starts
the application with the right environment, whithout needing you to source it
yourself. This way, you don't mess up your shell.

```javascript
'use strict'

let compile = run({
  sh: 'stack build',
  watch: 'app/**'
})

let server = runServer({
  httpPort,
  env: { "PORT": httpPort },
  sh: `./path/to/exe`
}).dependsOn(compile)

proxy(server, 8080).dependsOn(compile)
```

## Deploying on Clever Cloud

That's the easiest part. Once you've made sure the generated application
listens on `$PORT`, everything works out of the box: dependencies fetching (and
caching), compilation, deployment.

	clever create -t haskell my-homepage
	clever deploy

## Wrap up

I've played a lot with web stacks these last years, and now I know what I like:
a small set of independent libraries working reasonably well together, providing

 - routing
 - templating if needed
 - serde (without reflection)

Scotty, aeson and blaze-html (or lucid) provide exactly what I need, on top of
the rock-solid warp server. Thanks to WAI, there are lots of middlewares I can
use to save time (metrics, CORS, usw).

Next time we'll see how to broadcast a URL with the physical web API.
