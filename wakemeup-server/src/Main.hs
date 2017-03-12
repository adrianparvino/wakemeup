-- | This file is part of wakemeup
-- |
-- | wakemeeup is free software: you can redistribute it and/or modify
-- | it under the terms of the GNU General Public License as published by
-- | the Free Software Foundation, either version 3 of the License, or
-- | (at your option) any later version.
-- |
-- | wakemeup is distributed in the hope that it will be useful,
-- | but WITHOUT ANY WARRANTY; without even the implied warranty of
-- | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- | GNU General Public License for more details.
-- |
-- | You should have received a copy of the GNU General Public License
-- | along with wakemeup.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Web.Spock hiding (SessionId)
import Web.Spock.Config
import Control.Monad
import Data.IORef
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Config

main = do
  ref <- newIORef False
  spockCfg <- defaultSpockCfg () PCNoDatabase ref
  app <- spockAsApp $ spock spockCfg commentSystem
  runTLS (tlsSettings tlsCert tlsKey) (setPort 443 $ defaultSettings) app

commentSystem :: SpockM () () (IORef Bool) ()
commentSystem = do
  get "toggle" $ do
    ref <- getState
    liftIO $ modifyIORef ref not
    val <- liftIO (readIORef ref)
    text $ if val
      then "Waking"
      else "Stopping"
  get "state" $ do
    getState >>= liftIO . readIORef >>= flip when (setStatus status404)
  return ()
