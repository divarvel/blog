---
title: A tale of servant clients
author: Clement Delafargue
tags: haskell, servant
---

At Fretlink, we use [Servant](ToDo) *a lot*. All of our haskell webservices
are written using servant-server, which is a pure joy to use (see my post on
[macaroon-based auth](TODO)).

In addition to servant-server, we also use servant-client, to query the services
implemented with servant-server as well as external services (currently, PipeDrive).

While just using servant-server is quite pleasant, raw servant-client use can get complicated.

Let's consider a simple example:

TODO output of debugLayout.

```haskell
type UserEndpoints =
       Get '[JSON] User
  :<|> ReqBody '[JSON] UserData :> Put '[JSON] User

type UserAPI = "users" :>
       Get '[JSON] [User]
  :<|> ReqBody '[JSON] UserData :> Post '[JSON] User
  :<|> Capture "user_id" UserId :> UserEndpoints

type API = RequireMacaroon :> UserAPI

api :: Proxy API
api = Proxy
```

Extracting clients for this API can look like that

```haskell
listUsers :: AuthenticatedRequest RequireMacaroon
          -> ClientM [User]
listUsers auth =
  let (lu :<|> _) :<|> _ = client api auth
  in lu

editUser :: AuthenticatedRequest RequireMacaroon
         -> UserId
         -> UserData
         -> ClientM [User]
editUser auth userId =
  let (_ :<|> _ :<|> ues) = client api auth
      (_ :<|> eu) = ues userId
  in eu
```

This not very pleasant to read or write: the pattern matching needed to extract endpoints generates quite a lot of syntactic noise, and you need to apply parameters here and there to get access to the inner values.

Fortunately, [servant-generic](todo) lets you avoid all this tedious pattern-matching and can generate records containing all the endpoints.

For the API we've seen above, we could declare the accompanying records, with some generic magic sprinkled on top of it:

```haskell
type APIClient = AuthenticatedRequest RequireMacaroon
              -> UserClient

data UserClient
  = UserClient
  { listUsers :: ClientM [User]
  , addUser   :: UserData -> ClientM User
  , withUser  :: UserId -> UserEndpointsClient
  }
  deriving GHC.Generic
  deriving anyclass SOP.Generic

instance (Client ClientM UserAPI ~ client)
  => ClientLike client UserClient

data UserEndpointsClient
  = UserEndpointsClient
  { getUser  :: ClientM User
  , editUser :: UserData -> ClientM User
  }
  deriving GHC.Generic
  deriving anyclass SOP.Generic

instance (Client ClientM UserEndpoints ~ client)
  => ClientLike client UserEndpointsClient
```

With all that done, you can get a client and call stuff with it:

```haskell
newClient :: APIClient
newClient = mkClient $ client @API Proxy

getUserIO :: ClientMacaroon -> UserId -> IO User
getUserIO macaroon userId =
  let UserClient{..} = newClient $ mkAuthReq macaroon
      UserEndpointsClient{..} = withUser userId
  in runClient $ getUser
```

Well… it's a bit better than previously, but not *way* better. Instead of using `RecordWildCards`, we could use function composition instead.

```haskell
getUserIO' :: ClientMacaroon -> UserId -> IO User
getUserIO' macaroon userId =
  runClient .
  getUser .
  (\ue -> ue userId) . -- or ($ userId) if you're lambda-intolerant
  withUser .
  newClient $ mkAuthReq macaroon
```

Still tedious, and with deep routes with multiple captured parameters, it gets worse. I still like the idea of chaining functions, as it resembles how the URI is built.

While right-to-left composition is a great choice in many cases (and don't get me started on why everyone uses `>>=` instead of the clearly superior `=<<`), in that case, following the URL more closely seems better. Also, as much as I love abusing sections in the pursuit of η-reduction, `($ userId)` is not particularly readable. That being said, using lambdas is a definite no-no, so we'll add a few helpers.


```haskell
-- | Instead of passing the macaroon explicitly, we're
-- taking it as a dependency.
type Application a = ReaderT ClientMacaroon IO a

-- | Helper managing the client creation for us, as well
-- as constructing an @AuthenticatedRequest@ with the 
-- macaroon retrieved from the environment.
-- Don't pay too much attention to the implementation
runWithAuth :: (UserClient -> ClientM a)
            -> Application a
runWithAuth f = do
  client <- asks (newClient . mkAuthReq)
  liftIO . runClient . f $ client

-- | This helper is there purely to improve readability.
-- We could also define other versions with different arities
-- (even though chaining multiple calls to @WithParam@ would
-- work as well
withParam :: a -> (a -> b) -> b
withParam = flip ($)

editUserIO'' :: UserId -> UserData -> Application User
editUserIO'' userId userData = runWithAuth $
    withUser >>> withParam userId >>> editUser >>> withParam userData
    -- We could also completely forego @withParam@ with the backquote trick
    -- This is definitely shorter, but arguably less readable
    -- (`withUser` userId) >>> (`editUser` userData)
```

If you ask me, it's a bit easier on the eye.
