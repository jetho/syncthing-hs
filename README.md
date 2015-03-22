
syncthing-hs 
============

[![Hackage](https://img.shields.io/hackage/v/syncthing-hs.svg)](https://hackage.haskell.org/package/syncthing-hs)
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

import Control.Lens ((&), (.~), (?~))
import Control.Monad (liftM2)
import qualified Network.Wreq as Wreq
import Network.Syncthing
import qualified Network.Syncthing.Get as Get

-- A single Syncthing request.
single = syncthing defaultConfig Get.ping

-- Connection sharing for multiple Syncthing requests.
multiple1 = withManager $ \cfg ->
    syncthing cfg $ do
        p <- Get.ping
        v <- Get.version
        return (p, v)

-- Multiple Syncthing requests with connection sharing and customized configuration.
multiple2 = withManager $ \cfg -> do
    let cfg' = cfg & pServer .~ "192.168.0.10:8080"
                   & pHttps  .~ True
                   & pAuth   ?~ Wreq.basicAuth "user" "pass"
    syncthing cfg' $ liftM2 (,) Get.ping Get.version
```

