
syncthing-hs 
============

[![Build Status](https://travis-ci.org/jetho/syncthing-hs.svg?branch=master)](https://travis-ci.org/jetho/syncthing-hs)

Haskell bindings for the [Syncthing REST API](https://github.com/syncthing/syncthing/wiki/REST-Interface).

Tutorial
--------

A short tutorial is available at: 
[http://jetho.org/posts/2015-03-07-syncthing-hs-tutorial.html](http://jetho.org/posts/2015-03-07-syncthing-hs-tutorial.html)


Installation
------------

    cabal update
    cabal install syncthing-hs

Usage Example
-------------

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Wreq as Wreq
import Control.Monad (liftM2)
import Control.Lens ((&), (.~), (?~))
import Network.Syncthing
import Network.Syncthing.Get (ping, version)

-- A single Syncthing request.
single = syncthing defaultConfig ping

-- Connection sharing for multiple Syncthing requests.
multiple1 = withManager $ \cfg ->
    syncthing cfg $ do
        p <- ping
        v <- version
        return (p, v)

-- Multiple Syncthing requests with connection sharing and customized configuration.
multiple2 = withManager $ \cfg -> do
    let cfg' = cfg & pServer .~ "192.168.0.10:8080"
                   & pHttps  .~ True
                   & pAuth   ?~ Wreq.basicAuth "user" "pass"
    syncthing cfg' $ liftM2 (,) ping version
```

