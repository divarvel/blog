---
title: a (new) tale of servant clients
author: Clement Delafargue
tags: haskell, servant, fretlink
---

I was super happy with [my previous post about servant clients](../posts/2018-12-27-a-tale-of-servant-clients.html)… And then, we upgraded to servant
0.16… which dropped entirely the generic mechanism we used (to be fair, it was not documented, so…).

Lo and behold, we migrated to the proper [servant generic](http://hackage.haskell.org/package/servant-0.16.2/docs/Servant-API-Generic.html) system.

Instead of declaring the API types _and then_ replicating them in records,
we directly declare the API types with records.

```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
module API where

import           Servant
import           Servant.API.Generic
import           Servant.Client.Generic

import           Types                  (User, UserData)

type API = BasicAuth "user-management" User :> ToServantApi UsersAPI

data UsersAPI r
  = UsersAPI
  { getUsers :: r :- Get '[JSON] [User]
  , postUser :: r :- ReqBody '[JSON] UserData :> Post '[JSON] NoContent
  , withUser :: r :- Capture "user_id" UserId :> ToServantApi UserAPI
  }
  deriving Generic

data UserAPI r
  = UserAPI
  { getUser    :: r :- Get '[JSON] User
  , putUser    :: r :- ReqBody '[JSON] UserData :> Put '[JSON] NoContent
  , deleteUser :: r :- Delete '[JSON] NoContent
  }
  deriving Generic
```

The magical part here is the `r`. Choosing the `r` allows us to get a record
of api types, server types, or client types: we define everything in one place
(the records), and then instantiate their fields to what we want:

- `UsersAPI AsAPI` gives us a record of API types
- `UsersAPI AsServer` gives us a record of Server types (the handlers)
- `UsersAPI (AsClientT ClientM)` gives us a record of client types
   (functions taking parameters and returning ClientM values)

Notice that since record fields require "regular" servant API types, we can't
nest records directly as we did previously.

We need to turn the _record of API types_ describing the sub-api into a
_regular API type_.  To do that, we can use `ToServant` to go from the
records to the good old servant types.

We can get:

- the API type with `ToServantApi` (a shortcut for `ToServant UsersAPI AsAPI`)
- the server type with `ToServant UsersAPI AsServer`
- the client type with `ToServant UsersAPI (AsClientT ClientM)`

So at the _type_ level, we can use _ToServant_ to go from the record types
to the regular types. At the _value_ level, we can go from the regular types
to the record types, with `fromServant`. It may seem strange, but it makes
sense: _we_ are responsible for providing the _types_ (and we do so with
records). From records, we go to types that servant can understand (with
`ToServant`). With those types, _servant_ is responsible for providing a
client, that _we_ want to use as a record.

So given a regular servant client (here, `ToServant UsersAPI (AsClientT
ClientM)`), we can get a _record of clients_ (here, `UsersAPI (AsClientT
ClientM)`). Instead of calling `fromServant` on `client`, we can directly use
`genericClient` to get a record of clients.

It's all good and well, except for a small difference: in the new way, you
don't nest records, you just reference regular types (that you generate from
records with `ToServantApi`).

In the previous system, since we had nested records, we were able to compose
accessors like this: 

```haskell
withUser >>> ($ userId) >>> putUser >>> ($ userData)
```

Now, every time we call a record accessor, we don't get the next record,
but a _regular_ servant client. So we have to interleave `fromServant` calls
between record accessors as such:

```haskell
withUser >>> ($ userId) >>> fromServant >>> putUser ($ userData)
```

Not really satisfying.

However, with a couple helpers, we can improve it substantially:

```haskell
(//) :: ((m ~ AsClientT n), GenericServant r m)
     => (a -> ToServant r m)
     -> (routes m -> b)
     -> (a -> b)
f // f' = f >>> fromServant >>> f'

(/:) :: (a -> b -> c)
     -> b
     -> a -> c
(/:) = flip
```

`//` interleaves accessor calls with `fromServant`, while `/:` hides the `(>>>
($ p))` pattern. I chose those names due to the similarity with the way routes
are declared in many frameworks (with the leading `:` for captured parameters).

With all that done, we can go back to a nice style:

```haskell
withUser /: userId // putUser /: userData
```

I've been told that servant generic records don't compose elegantly;
I hope I've shown they _can_.

## Addendum

I kept the first part short and to the point, but I wanted to illustrate a
bit more what's going on behind the scenes.

You may think this is all magic, as Servant relies _heavily_ on type families,
which hide the "actual" types you're dealing with. Fear not, we can see
precisely what's happening with `:kind!`. If you want to follow along, you can
download a [working example](../files/servant-client.hs) (it's a stack executable
file, if you run it, it will drop you in a GHCI session with everything in scope).

Let's see the regular API type derived from `UsersAPI`:

```
λ> :kind! ToServantApi UsersAPI
ToServantApi UsersAPI :: *
= Verb 'GET 200 '[JSON] [User]
  :<|> ((ReqBody '[JSON] UserData :> Post '[JSON] NoContent)
        :<|> (Capture "user_id" UserId
              :> (Verb 'GET 200 '[JSON] User
                  :<|> ((ReqBody '[JSON] UserData :> Put '[JSON] NoContent)
                        :<|> Verb 'DELETE 200 '[JSON] NoContent))))
```

And now, the regular client type derived from `UsersAPI`.

```
λ> :kind! ToServant UsersAPI (AsClientT ClientM)
ToServant UsersAPI (AsClientT ClientM) :: *
= ClientM [User]
  :<|> ((UserData -> ClientM NoContent)
        :<|> (Int
              -> ClientM User
                 :<|> ((UserData -> ClientM NoContent) :<|> ClientM NoContent)))
```

That should help you getting by with the `ToServant` type family.

Now, if you want to understand more about the record types, `:kind!` will
not help you _directly_. Since those records are… well, records, and not
type families you won't get interesting results:

```
λ> :kind UsersAPI
UsersAPI :: * -> *
λ> :kind UsersAPI AsApi
UsersAPI AsApi :: *
```

The records themselves keep the same structure, what changes is the types
of their fields. Notice the `:-` bit?

```
λ> :kind! AsApi :- Get '[JSON] [User]
AsApi :- Get '[JSON] [User] :: *
= Verb 'GET 200 '[JSON] [User]

λ> :kind! AsServer :- Get '[JSON] [User]
AsServer :- Get '[JSON] [User] :: *
= Handler [User]
```

One last word: if the compiler complains of a mismatch between _something that
looks like an API type_ and _something that looks like a server or a client
type_, make sure you've not forgotten the `r :-` in your record fields :-)
