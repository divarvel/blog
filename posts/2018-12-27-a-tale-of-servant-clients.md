---
title: A tale of servant clients
author: Clement Delafargue
tags: haskell, servant, fretlink
---

At Fretlink, we use
[Servant](https://haskell-servant.readthedocs.io/en/stable/) *a
lot*. All of our haskell webservices are written using servant-server,
which is a pure joy to use (see my post on [macaroon-based
auth](http://blog.clement.delafargue.name/posts/2018-07-19-bake-delicious-macaroon-burritos-with-servant.html)).

In addition to servant-server, we also use servant-client, to query the
services implemented with servant-server as well as external services
(currently, PipeDrive).

While just using servant-server is quite pleasant, raw servant-client use
can get complicated.

Let's consider a small user management API, protected by `basic auth`:

```
/
└─ users/
   ├─• GET
   ├─• POST
   └─ :user_id /
      ├─• GET
      ├─• PUT
      └─• DELETE
```

Transcribed in the servant DSL, we get:

```haskell
type API = BasicAuth "user-management" User :> UsersAPI

type UsersAPI = "users" :>
       Get '[JSON] [User]
  :<|> ReqBody '[JSON] UserData :> Post '[JSON] NoContent
  :<|> Capture "user_id" UserId :> UserAPI

type UserAPI =
       Get '[JSON] User
  :<|> ReqBody '[JSON] UserData :> Put '[JSON] NoContent
  :<|> Delete '[JSON] NoContent
```

Extracting clients for this API can look like that:

```haskell
listUsersClient :: BasicAuthData
          -> ClientM [User]
listUsersClient auth =
  let lu :<|> _ = client api auth
  in lu

editUserClient :: BasicAuthData
         -> UserId
         -> UserData
         -> ClientM [User]
editUserClient auth userId =
  let _ :<|> _ :<|> ue = client api auth
      _ :<|> eu :<|> _ = ue userId
  in eu
```

This not very pleasant to read or write: the pattern matching needed to
extract endpoints generates quite a lot of syntactic noise, and you need to
apply parameters here and there to get access to the inner values.

Fortunately, servant supports generic derivation for clients.

For the API we've seen above, we can declare the accompanying records,
with some generic magic sprinkled on top of it:

```haskell
type APIClient = BasicAuthData
              -> UserClient

data UsersAPIClient = UserClient
  { listUsers :: ClientM [User]
  , addUser   :: UserData -> ClientM NoContent
  , withUser  :: UserId -> UsersAPIClient
  }
  deriving stock GHC.Generic -- (from GHC.Generics)
  deriving anyclass SOP.Generic -- (from Generics.SOP)

instance (Client ClientM UsersAPI ~ client)
  => ClientLike client UsersAPIClient

data UserAPIClient = UserAPIClient
  { getUser  :: ClientM User
  , editUser :: UserData -> ClientM NoContent
  }
  deriving stock GHC.Generic -- (from GHC.Generics)
  deriving anyclass SOP.Generic -- (from Generics.SOP)

instance (Client ClientM UserAPI ~ client)
  => ClientLike client UserAPIClient
```

With all that done, you can get a client and call stuff with it:

```haskell
newClient :: APIClient
newClient = mkClient $ client api

listUserClient' :: BasicAuthData -> ClientM [User]
listUserClient' auth = listUsers $ newClient auth

editUserClient' :: BasicAuthData
                -> UserId -> UserData
                -> ClientM NoContent
editUserClient' auth userId userData =
  editUser (withUser (newClient auth) userId) userData
```

Well… it's a bit better than previously, but not *way*
better. `listUserClient'` is not too bad, but for `editUserClient'`, it's
quite hard to read (and come up with). See how `userId` and `userData`
are far from `withUser` and `editUser`? It only gets worse with bigger routes.

With a little trick, we can fix that:

```haskell
editUserClient'' :: BasicAuthData
                -> UserId -> UserData
                -> ClientM NoContent
editUserClient'' auth userId userData =
  ($ userData) . editUser . ($ userId) . withUser $ newClient auth
```

You can also use a lambda if you're allergic to sections (replace `($ userId)`
with `(\e -> e userId)`). It's still tedious, but at least it keeps parameters
application close to where they're defined.

Another solution is to use `RecordWildCards`.

```haskell
editUserClientWithWildCards :: BasicAuthData
                            -> UserId -> UserData
                            -> ClientM NoContent
editUserClientWithWildCards auth userId userData =
    let UsersAPIClient{..} = newClient auth
        UserAPIClient{..} = withUser userId
    in editUser userData
```

This also prevents the parameters from going too far, but I find it quite
verbose. There is little syntactic noise, but I think it obscures the URL
structure.

Thankfully, we can still improve on building routes with function application.
While right-to-left composition is a great choice in many cases [^1], in
this context, following the URL more closely seems better. Also, as much as
I love abusing sections in the pursuit of η-reduction, `($ userId)` is not
particularly readable. That being said, using lambdas is a definite no-no,
so we'll add a few helpers.

Since manually passing `BasicAuthData` everywhere is tedious, we'll apply
*Entreprise FP Patterns* and use a `Reader` to handle this. While we're at
it, we can also get a `ClientEnv` from the reader and actually run the request.

```haskell
data ApplicationEnv = ApplicationEnv
  { auth :: BasicAuth
  , env :: ClientEnv
  }

-- | Instead of passing BasicAuthData explicitly, we're
-- taking it as a dependency, as well as the HTTP client environment
-- (base URL, HTTP client manager, cookies, …)
type Application a = forall m. (MonadReader ApplicationEnv m, MonadIO m) => m a

-- | Helper managing the client creation for us, as well
runClient :: (UsersClient -> ClientM a)
          -> Application (Either ServantError a)
runClient f = do
  ApplicationEnv{..} <- ask
  liftIO $ runClientM (f $ newClient auth) clientEnv

-- | This helper is there purely to improve readability.
-- We could also define other versions with different arities
-- (even though chaining multiple calls to @WithParam@ would
-- work as well
withParam :: a -> (a -> b) -> b
withParam = flip ($)
```

With all this done, and thanks to `(>>>)` from `Control.Category`,
we can improve readability:

```haskell
editUserIO :: UserId -> UserData -> Application User
editUserIO userId userData = runWithAuth $
    withUser >>> withParam userId >>> editUser >>> withParam userData

    -- We could also completely forego @withParam@ with the backquote trick.
    -- This is definitely shorter, but arguably less readable
    -- (`withUser` userId) >>> (`editUser` userData)

    -- If you're not allergic to dollar sections, it reads quite well.
    -- withUser >>> ($ userId) >>> editUser >>> ($ userData)
```

In the codebase I am working on, I went with a `>>>` / `withParam` combo,
and I am quite happy with the result. Maybe the same could be achieved with
lenses, but I have not tried it yet.

This article uses servant clients as an example, but it could have been the
same with any library. In the end, we're composing functions, no more, no
less.

I really like the way everything comes together nicely when you have found
the right way to combine values. The experience of refactoring haskell code,
especially with hlint by my side hasn't been matched in any other language
for now.

[^1]: Don't get me started on why everyone uses `>>=` instead of the clearly
  superior `=<<`
