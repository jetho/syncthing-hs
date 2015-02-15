
syncthing-hs 
============

[![Build Status](https://travis-ci.org/jetho/syncthing-hs.svg?branch=master)](https://travis-ci.org/jetho/syncthing-hs)

Haskell bindings for the Syncthing REST API, see [https://github.com/syncthing/syncthing/wiki/REST-Interface](https://github.com/syncthing/syncthing/wiki/REST-Interface)

Installation
------------

    cabal update
    cabal install syncthing-hs

Usage Examples
--------------

The following environment is assumed for all examples:

 ```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((&), (.~), (?~))
import qualified Network.Wreq as W

import Network.Syncthing
import qualified Network.Syncthing.Get as Get
import qualified Network.Syncthing.Post as Post
 ```
 
### The default Syncthing configuration

 ```haskell
λ: print defaultConfig
SyncConfig { pServer = "127.0.0.1:8080", pApiKey = Nothing, pAuth = Nothing, pHttps = False, pManager = Left _ }
 ```
 
### Customizing the default Syncthing configuration

 ```haskell
λ: let cfg = defaultConfig & pServer .~ "192.168.0.10:8080"
                           & pApiKey ?~ "XXXX"
                           & pHttps  .~ True
                           & pAuth   ?~ W.basicAuth "user" "pass"
λ: print cfg
SyncConfig { pServer = "192.168.0.10:8080", pApiKey = Just "XXXX", pAuth = Just (BasicAuth "user" "pass"), pHttps = True, pManager = Left _ }
 ```

### A single Syncthing request

 ```haskell
syncthing defaultConfig Get.ping
 ```
 
### Connection sharing for multiple requests

Running multiple requests with the default configuration is somewhat inefficient since a new connection manager is created for each request. It's recommended using the withManager* functions for connection sharing among multiple requests.

 ```haskell
withManager $ \cfg ->
    syncthing cfg $ do
        p <- Get.ping
        v <- Get.version
        return (p, v)
 ```
 
### Multiple requests with connection sharing and customized configuration

 ```haskell
import Control.Monad (liftM2)
 
withManager $ \cfg -> do
    let cfg' = cfg & pServer .~ "192.168.0.10:8080"
                   & pHttps  .~ True
                   & pAuth   ?~ W.basicAuth "user" "pass"
    syncthing cfg' $ liftM2 (,) Get.ping Get.version
 ```
 
### Requests with disabled SSL certificate verification 

 ```haskell
import Control.Monad (liftM2)
 
withManagerNoVerify $ \cfg -> do
    let cfg' = cfg & pHttps .~ True
    syncthing cfg' $ liftM2 (,) Get.ping Get.version
 ```
 
### Post requests

 ```haskell 
λ: let cfg = defaultConfig & pApiKey ?~ "XXXX"
λ: syncthing cfg $ Post.sendError "Error 1"
Right ()
λ: syncthing cfg $ Post.scanFolder Post.defaultFolder (Just "foo/bar")
Right ()
λ: syncthing cfg $ Post.restart
Right (Just Restarting)
 ```
 
### Manual session handling

Manual session handling is convenient if you need longer-lived sessions instead of "one-shot" requests.
You can create a session by applying "newSyncSession" to your syncthing configuration.
Because of connection sharing you should reuse the session whenever possible.
The session has to be closed if it isn't needed anymore.

 ```haskell 
import Network.Syncthing.Session

-- Customized configuration with disabled SSL certificate verification.
cfg = defaultConfig & pHttps   .~ True
                    & pManager .~ Left noSSLVerifyManagerSettings

result = do
    session <- newSyncSession cfg
    p       <- runSyncSession session Get.ping
    v       <- runSyncSession session Get.version
    closeSyncSession session
    return (p, v)
 ```

