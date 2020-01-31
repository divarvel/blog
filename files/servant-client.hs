#!/usr/bin/env stack
-- stack --resolver lts-14.3 --install-ghc ghci --package servant --package servant-client --package warp --package servant-server --package text --package aeson --package mtl --package wai --package servant-client-core
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Category         ((>>>))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ask)
import           Data.Aeson
import qualified Data.Text.IO             as TIO
import           GHC.Generics             (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

type UserId = Int
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UserData = UserData
  { firstName :: String
  , lastName  :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

(//) :: ((m ~ AsClientT n), GenericServant r m)
     => (a -> ToServant r m)
     -> (r m -> b)
     -> (a -> b)
f // f' = f >>> fromServant >>> f'

(/:) :: (a -> b -> c)
     -> b
     -> a -> c
(/:) = flip

type UsersAPI' = UsersAPI (AsClientT ClientM)

-- helper that generates the `UsersAPI` record and lets
-- you apply a function on it
runAuth :: BasicAuthData
        -> (UsersAPI' -> ClientM a)
        -> ClientM a
runAuth auth f =
  let usersAPI = fromServant $ client @API Proxy auth
  in f usersAPI

-- example of how you can declare a request, thanks to
-- `runAuth` (note that you could use `MonadReader` instead
-- of passing `BasicAuthData` directly
updateUser :: BasicAuthData
           -> UserId
           -> UserData
           -> ClientM NoContent
updateUser auth uid payload =
  runAuth auth $ withUser /: uid // putUser /: payload

main :: IO ()
main = putStrLn "hi"
