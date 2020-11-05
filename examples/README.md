Examples of Effectful Streaming For Idris
=====

The following examples assume that your first step was to enter the `examples` directory and type `idris2 --build examples.ipkg`

* `File.idr` shows an example of stream processing on a file including running decoding.  
  It is most directly ran by typing `idris2 -p streaming -p managed File.idr --exec main`  
  Alternatively you can type `idris2 --repl examples.ipkg` and then `:module File` and finally `:exec main`

* `Base64.idr` is a test of stream encoding/decoding on known and unknown data.  
  It is most directly ran by typing `idris2 -p streaming -p managed Base64.idr --exec main`  
  Alternatively you can type `idris2 --repl examples.ipkg` and then `:module Base64` and finally `:exec main`

* `Network.idr` has an example of connecting to a website and streaming the results to stdout and a file at the same time.  
It is most directly ran by typing `idris2 -p network -p streaming -p managed Network.idr --exec main`  
Alternatively you can type `idris2 --repl examples.ipkg` and then `:module Network` and finally `:exec main`

This currently installs `streaming` when built (for my own convenience) comment out the 'prebuild' line in examples.ipkg if you don't want this to happen.

Version
-------

This package follows [Haskell PVP](https://pvp.haskell.org/) which is distinct from [SEMVER](https://semver.org/) in that when examining `1.2.3`, `1.2`  is the Major Version rather than `1`.
