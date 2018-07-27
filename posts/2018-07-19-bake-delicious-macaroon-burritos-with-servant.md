---
title: Bake Delicious Macaroon Burritos With Servant
author:  Clément Delafargue
tags: haskell, servant, macaroons, fretlink
---

When I've joind the Fretlink team in May, I've had the pleasure to find a team
already enjoying the use of macaroons. One of my first missions was to improve
how they were used to authorize API calls, in the context of Servant
applications. After a few iterations, we've settled on a design I'm quite happy
with.

This involves Servant's generalized auth mechanism, monad transformers, natural
transformations, and, something I've called _deferred macaroons validation_.

Hop on for a quick tour on how we did it, why we did it and what's left ahead!

<!-- more -->

## What are macaroons?

Macaroons are _bearer tokens_, initially conceived to allow sharing with
contextual caveats. Let's unpack it a bit: they're _bearer tokens_, which means
they're meant to be sent with a request to authorize it. The service receiving
the request can check the token to allow it to proceed or not.

Now for the interesting part: _contextual caveats_. Macaroons can be
_attenuated_ with something called caveats. In layman terms, it means that a
macaroon can be attenuated by adding "this is valid as long as condition X
holds". When receiving a macaroon along with a request, the service has to
prove every caveat holds.

A macaroon looks like this:

```
identifier <opaque_id>   # opaque user id. Permissions will be scoped to it
location <location hint> # url where you can use the macaroon
cid endpoint = route1    # caveats are just strings
cid time < 2018-07-18Z
signature <signature>    # allows to check the macaroon validated
```

In practice, when verifying a macaroon, you provide a list of verifiers
(roughly `Caveat -> IO (Either VerifierError ())`, with `data VerifierError = Unrelated | Error String`). Each verifier is then applied to each caveat, and
validation succeeds if every caveat has at least one verifier completing
successfully. Most verifiers (called _exact verifiers_) just look for
exact strings. For instance, if you're hitting the `route1` endpoint,
the corresponding verifier will just match the exact string `endpoint = route1`.
For the TTL caveat, the verifier will parse the date string and succeed only
if its after the current time. The syntax `name =` or `name <` is purely arbitrary,
as far as the macaroon is concerned, caveats are strings. It's up to the verifiers
to parse (or not) the caveats when verifying them.

This gives macaroons two interesting properties:

- you can freely add any caveat to a macaroon, as it renders it _less_
  powerful.
- macaroons validation works in reverse compared to common intuition: a
  macaroon with no restrictions is almighty: you need to be careful when
  creating a macaroon, but when you validate one, there is no risk of
  forgetting to check something, since the checklist is embedded in the
  macaroon itself.

To learn more on macaroons, you can

- read the [original paper](https://ai.google/research/pubs/pub41892)
- read [libmacaroon's
  readme](https://github.com/rescrv/libmacaroons/blob/master/README)

In Haskell, there is [hmacaroons](https://github.com/jtanguy) which implements
it for us.

## Auth management in web frameworks

Most web frameworks implement authorization and authentication as a sort of
filter. Usually, authentication can be done in a single place and provides a
context used by the endpoints. Authorization is split in two places: some
properties can be checked at a global level (token integrity, ttl, stuff like
that), but other properties can only be checked at the endpoint level (read or
write operation, rights matrix lookup, things like that).

While frameworks provide interesting tools for authorization and global
authorization, developers are often on their own when it comes to
endpoint-specific authorization concerns. What's worse is that it's unsecure by
default: developers _have_ to _add_ verifications, to make the endpoint
correctly secure.

That's where macaroons help us: we can make endpoints secure by default, by
making developers have to _prove_ that the request is compatible with the given
macaroon.

## Servant

To create our HTTP services, we use
[Servant](https://haskell-servant.readthedocs.io/en/stable/). Servant is a
"Type-Level Web DSL". That means, that the first step is to describe your API.
Then, you can either let servant derive clients, or you can provide handlers
for each endpoint, and servant will build a
[WAI](http://hackage.haskell.org/package/wai) application out of it.

A simple servant example looks like this:

```haskell
type API =
       ("route1" :> Get '[JSON] Resource)
  :<|> ("route2" :> Get '[JSON] OtherResource)

api :: Proxy API
api = Proxy

handleRoute1 :: Handler Resource
handleRoute1 = pure myResource

handleRoute2 :: Handler OtherResource
handleRoute2 = pure myOtherResource

-- Handlers can be composed, this allows you to compose sub-APIs
-- and even abstract away common patterns
server :: Server API
server = handleRoute1 :<|> handleRoute2

-- Generates a WAI application from an API description and corresponding
-- handlers
app :: Application
app = serve api server
```

What I like about servant is that there is still a DSL to describe the routes,
instead of having annotations directly on handlers. You can have a quick look
at the type and know the structure of the API. Instead of relying on an
external DSL like Play Framework does, you get proper haskell, so you can mix
and match API fragments freely, without arbitrary restrictions. The other cool
thing is that it lets you think in terms of high-level types, and never worry
about serialization, while still giving you complete control about it.

### Servant's generalized authentication

Like everything, authentication has to show up in types:

```haskell
type ProtectedAPI = RequireMacaroon :> "route1" :> Get '[JSON] Resource
```

Once an enpoint is marked as protected, the handler function will take a new
argument, representing the information extracted from the request, usually a
`User` or `Account` data type. In our cases, it was initially a `Macaroon`.

```haskell
protectedServer :: Server ProtectedAPI
protectedServer macaroon = myHandler macaroon

myHandler :: Macaroon -> Resource
myHandler macaroon = …
```

To tie everything up, we still need to tell servant server how to extract it
from the request, in the form of something like `Request -> Handler Macaroon`
(see [Servant Generalized Authentication
docs](https://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html#generalized-authentication)
for more info).

This allows us to handle authentication (with the macaroon's key identifier),
as well as the verification of things like TTL. This still doesn't account for
_contextual_ caveats since we don't know about the endpoint being hit. We could
extract information from the request but that would force us to duplicate all
the endpoint description logic in this handler, losing all the type-safe
goodness servant provides.

Another possibility would be to extract the macaroon without any validation in
the auth handler, and have every endpoint do all the verifications, with a
complete context. That's not satisfying either, as it would make endpoints
insecure by default.

## Deferred validation

What we want is to validate what we can in the auth handler, while waiting for
more context provided by the endpoint. This gives rise to what we can call
deferred validation.

Some caveats are marked as eligible for deferred validation and are
unconditionally discharged in the auth handler. Caveats not properly discharged
are kept alongside the macaroon and will have to be discharged later on.

```haskell
data PartiallyValidatedMacaroon = PartiallyValidatedMacaroon
  { macaroon :: Macaroon
  , remainingCaveats :: [Caveat]
  }
```

The last piece is about verifying the remaining caveats while staying secure
by default. To do that, we give a way to annotate handlers with verifiers, and
we match those verifiers against the remaining caveats to let the request go
through, or reject it. The key of being secured by default is to force this
check. If it's done, since endpoints do provide any verifier unless explicitely
annotated, we get security by default.

To put that in motion, we'll use natural transformations and a nifty servant
mechanism called `hoistServer`. But first I'd like to have a word with you
about middlewares.

## Why middlewares are not enough

The concept of _middleware_ is common along web
frameworks and web servers, it's famously present in
[Express](http://expressjs.com/en/guide/using-middleware.html), as well as in
[WAI](http://hackage.haskell.org/package/wai-3.2.1.2/docs/Network-Wai.html#t:Middleware).
Play Framework has similar concepts, called _filters_ and _action builders_.

Middlewares typically work by inspecting a request, then either short-circuit
or pass the request (sometimes modified) to the rest of the application.

In express, an auth middleware looks like this:

```javascript
const authMiddleware = (request, response, next) => {
  const user = checkAuth(request);
  if (user) {
    // the request is modified
    request.user = user;
    next(r);
  } else {
    res.status(403).send({});
  }
};
```

In Play framework, it's a bit more involved, but it's quite close. The extra
complexity is needed for the types to reflect what's happening. Play also lets
you enforce properties about action builders: "does it shortcut", "does it
modify the request".

```scala
case class WithUser[A](user: User, request: Request[A]) extends WrappedRequest[A]
class Authenticated @Inject() (
    cc: ControllerComponents
) extends ActionRefiner[Request, WrappedRequest] {
  def refine[A](request: Request[A]): Future[Either[Result,WithUser[A]]] = {
    // perform auth and optionally return a modified request
  }
}
```

In WAI (as well in other frameworks in the rust ecosystem), a different
approach is used: instead of modifying or wrapping the request, a [type-indexed
data structure](https://github.com/HeinrichApfelmus/vault) is used. This allows
to modify requests while providing extensibility.

While it makes sense at the server-level (eg enabling gzip or handling CORS),
using middlewares for application-level concerns is unsatisfactory. Play
recognizes it by splitting _filters_ and _action composition_, but it's still
quite constrained.

Servant's generalized auth mechanism side steps this issue by putting auth
concerns into the API type and directly providing the auth data as an argument
to the handler. But as we've seen, it's not the only part of the story. We
still want to annotate endpoints (or whole API subtrees) to handle
authentication. That's where servant's design shines once again.

## Enter `hoistServer` 

By default, handlers live in the `Handler` monad. It provides `IO`
capabilities, as well as error capabilities. Servant allows you to write
handlers in another monad, as long as you provide a way to go from your monad
to the `Handler` monad. To do that, you can use `hoistServer` and provide a
function of type `forall a. MyMonad -> Handler a`.

In practice, all of the handlers live in `ReaderT Config Handler`, so we provide
a `ReaderT Config Handler a -> Handler a` transformation (more or less
`runReaderT`) to let handlers access global configuration. Some handlers even
leave in a `ReaderT Macaroon (ReaderT Config Handler)` to provide access to the
request macaroon without needing to thread it manually across all the API tree.
Haskell devs are lazy, don't you know?

We can use it to great effect by defining a wrapped type containing both our
handler and its annotated verifiers.

At first, I realized that verifiers were combined in a monoidal way, so I
thought I could just encode it with a `Writer`. Try to find the issue with that
:).

```haskell
-- rougly: PartiallyValidatedMacaroon -> m ([Verifier], a)
newtype MacaroonHandlerT m a =
  WriterT [Verifier]
    (ReaderT PartiallyValidatedMacaroon m) a

-- we can use Writer and the monoid on [] to compose verifiers
addVerifiers :: [Verifier] -> MacaroonHandlerT m a -> MacaroonHandlerT m a
addVerifiers verifiers = (tell verifiers >>)
```

When I realized the issue (more on that later), I came up with another design.
It's still flawed in a way (more on that later), but that's what I went with.

```haskell
data WithVerifiersT m a = WithVerifiersT { verifiers :: [Verifier], handler :: m a }
newtype MacaroonHandlerT m a = MacaroonHandlerT
  { runMacaroonHandlerT :: ReaderT PartiallyValidatedMacaroon (WithVerifiersT m) a
  }


-- Since we don't have a Writer anymore, we need to explicitely say:

instance (Applicative m) => Applicative (WithVerifiersT m) where
  -- #1: That by default, handlers don't have any verifier attached to it
  pure = WithVerifiers [] . pure
  -- #2: How verifiers can be combined
  (<*>) = error "left as an exercise for the reader"

-- #2: How verifiers can be added to existing handlers
addVerifiers :: [Verifier] -> MacaroonHandlerT m a -> MacaroonHandlerT m a
addVerifiers = error "left as an exercise for the reader"
```

The actual types are a bit more complicated, as they are polymorphic on
`[Verifier]`. We only require a monoid instance on `[Verifier]` for
composition.

Then we define a natural transformation `MacaroonHandlerT Handler a -> Handler a` (constructed by providing the extracted `PartiallyValidatedMacaroon`. Its
job is to match the remaining caveats with the provided verifiers, and either
return an error or execute the handler.

```haskell
toMacaroonHandler :: PartiallyValidatedMacaroon
                  -> MacaroonHandlerT Handler a
                  -> Handler a
toMacaroonHandler = error "apply the verifiers to the remaining caveats"
```

You may now see why the first try did not work: in order to get the verifiers
list, you need to run the handler. So much for security.

The issue with the second design is a bit trickier to see for the untrained
eye, but you can't define a law abiding monad with it. Try writing `instance Monad m => Monad (WithVerifiersT m)` to see for yourself (functor and
applicative instances can still be written).

It took me some time to figure those two issues (you can really get lost when
you're neck-deep in monad transformers mumbo-jumbo). The two issues stem from
the same problem: the data type representing a handler annotated with
verifiers can't be a monad: you need to statically declare all your
dependencies before going on with the other effects. This kind of static
properties are not compatible with monads.

Thankfully, natural transformations don't require monad instances. In the
actual implementation, only a functor instance is required.

A solution would be to not write a `Monad` instance for `WithVerifiersT` and to
force the users to wrap their handler definitions in a `pure` call if they
don't want to add verifiers. For now I've written an illegal monad instance, to
be able to use `do` blocks even when not adding verifiers, as well as
simplifying type inference a bit.

It's illegal because it forgets the new verifiers introduced by the inner
monadic value. It's still secure by default, even though it doesn't compose
like it's supposed to.

## Cherry on the cake

Natural transformations given to `hoistServer` need not return a `Handler` in
every case. You can freely chain natural transformations anywhere in your API
tree, as long as you end up with a `Handler` after the outermost
transformation.

In practice, that means you can annotate whole API trees with verifiers and
avoid repeating yourself at each leaf of your tree.

```haskell
type API = RequireMacaroon :> DummyEndpoints
type DummyEndpoints =
       "route1" :> Route1API
  :<|> "route2" :> Route2API

server :: Server API
server macaroon = hoistServer (Proxy @DummyEndpoints) (toMacaroonHandler macaroon) $
    r1Server :<|> r2Server
  where
    r1Server = hoistServer (Proxy @Route1API) (addVerifier isRoute1)
    r2Server = hoistServer (Proxy @Route2API) (addVerifier isRoute2)
```

This example makes use of the most excellent `TypeApplications` GHC extension to
avoid syntactic noise (`Proxy @Route1API` is roughly equivalent to `Proxy :: Proxy Route1API`)

The ability to chain `hoistServer` calls to annotate whole API subtrees has
helped simplify complex APIs a lot, while improving consistency. No need to tag
every handler manually!

## MTL

Writing the (bogus) monad transformers lead us to an interesting path down the
mtl-style monad transformers. But that's a story for another day.

## What I've learned along the way

I've been using haskell as my main language for less than two months, and at
some point I really felt I was out of my depth there. Thankfully, I always had
humans, GHC and hlint to watch my back. Special thanks to
[@Raveline](https://twitter.com/Raveline) and
[@alpmestan](https://twitter.com/alpmestan) who helped me navigate MTL and
Servant.

Servant's use of natural tranformations to add features to handlers is vastly
superior to what I've used in other frameworks where the main abstraction is
`Request -> Request`. You can use the type system to its full extent whithout
having to shoehorn things into request objects.

Using haskell full-time is even better than I expected. I've learned a lot,
especially about "boring" things that are usually eschewed in side projects.

Haskell is not just a super elegant language for FP weenies, it's also a solid
platform for professionnals.

