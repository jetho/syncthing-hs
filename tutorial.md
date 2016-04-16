# Tutorial

This is a short tutorial about the 
[syncthing-hs](https://github.com/jetho/syncthing-hs) package, which provides
Haskell bindings to the [Syncthing REST API](https://docs.syncthing.net/dev/rest.html).

For more details, please refer to the 
[Hackage documentation](https://hackage.haskell.org/package/syncthing-hs).


## Installation

Use cabal to install the 
[syncthing-hs package](https://hackage.haskell.org/package/syncthing-hs):

    cabal update
    cabal install syncthing-hs


## Usage Examples

The following environment is assumed for all examples:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((&), (.~), (?~))
import qualified Network.Wreq as W

import Network.Syncthing
import qualified Network.Syncthing.Get as Get
import qualified Network.Syncthing.Post as Post
```

### The default Syncthing configuration

The default Syncthing configuration contains information about the Syncthing 
server, Api Key, authentication etc.

``` haskell
λ: print defaultConfig
SyncConfig { server = "127.0.0.1:8384"
           , apiKey = Nothing
           , auth = Nothing
           , https = False
           , manager = Left _ 
           }
```
 
### Customizing the default Syncthing configuration

Customize the default Syncthing configuration to your needs by using record
syntax or the SyncConfig lenses:

``` haskell
λ: let cfg = defaultConfig { 
                server = "192.168.0.10:8080"
              , apiKey = Just "XXXX"
              , https  = True
              , auth   = Just $ W.basicAuth "user" "pass"
              }
λ: print cfg
SyncConfig { server = "192.168.0.10:8080"
           , apiKey = Just "XXXX"
           , auth = Just (BasicAuth "user" "pass")
           , https = True
           , manager = Left _ 
           }
```

``` haskell
λ: let cfg = defaultConfig & pServer .~ "192.168.0.10:8080"
                           & pApiKey ?~ "XXXX"
                           & pHttps  .~ True
                           & pAuth   ?~ W.basicAuth "user" "pass"
λ: print cfg
SyncConfig { server = "192.168.0.10:8080"
           , apiKey = Just "XXXX"
           , auth = Just (BasicAuth "user" "pass")
           , https = True
           , manager = Left _ 
           }
```

### A single Syncthing request

``` haskell
syncthing defaultConfig Get.ping
```
 
### Connection sharing for multiple requests

Running multiple requests with the default configuration is somewhat 
inefficient since a new connection manager is created for each request. 
If you are already using a connection manager elsewhere in your application, 
you can reuse the manager by updating the *manager* field of the configuration:

``` haskell
 let cfg = defaultConfig { manager = Right mgr }
```

If you don't have an active connection manager, it's recommended using the 
*withManager* function family for connection sharing among multiple requests.

``` haskell
withManager $ \cfg ->
    syncthing cfg $ do
        p <- Get.ping
        v <- Get.version
        return (p, v)
```
 
### Multiple requests with customized configuration

``` haskell
import Control.Monad (liftM2)
 
withManager $ \cfg -> do
    let cfg' = cfg & pServer .~ "192.168.0.10:8080"
                   & pHttps  .~ True
                   & pAuth   ?~ W.basicAuth "user" "pass"
    syncthing cfg' $ liftM2 (,) Get.ping Get.version
```
 
### Requests with disabled SSL certificate verification 

By default, the connection manager performs SSL certificate verification, which
may result in an error when connecting to a server with a self-signed 
certificate. The *withManagerNoVerify* function creates a manager with disabled
SSL certificate verification:

``` haskell
import Control.Monad (liftM2)
 
withManagerNoVerify $ \cfg -> do
    let cfg' = cfg & pHttps .~ True
    syncthing cfg' $ liftM2 (,) Get.ping Get.version
```
 
### Post requests

To use the Post methods, an API key must be set:

``` haskell 
λ: let cfg = defaultConfig & pApiKey ?~ "XXXX"
λ: syncthing cfg $ Post.sendError "Error 1"
Right ()
λ: syncthing cfg $ Post.scanFolder defaultFolder (Just "foo/bar")
Right ()
λ: syncthing cfg $ Post.restart
Right (Just Restarting)
```
 
### Manual session handling

Manual session handling is convenient if you need longer-lived sessions instead 
of "one-shot" requests.  You can create a session by applying *newSyncSession* 
to your syncthing configuration.
Because of connection sharing you should reuse the session whenever possible.
The session has to be closed when it isn't going to be used anymore.

``` haskell 
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

### Concurrent requests

You can run requests concurrently by using the 
[async package](https://hackage.haskell.org/package/async):

``` haskell 
λ: import Control.Concurrent.Async
λ: let pings = replicate 1000 Get.ping
λ: withManager $ \cfg -> mapConcurrently (syncthing cfg) pings
[Right "pong",Right "pong",Right "pong",Right "pong", ... ]
```

