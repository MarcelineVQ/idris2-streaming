Examples of Effectful Streaming For Idris
=====

Just one for the moment, but more to come and their development will influence the direction of the [streaming](https://github.com/MarcelineVQ/idris2-streaming) library. Especially in terms of how to work with Bits8/Bytes.

The following examples assume that your first step was to enter the `examples` directory and type `idris2 --build examples.ipkg`

* `File.idr` shows an example of stream processing on a file.  
  It is most directly ran by typing `idris2 -p streaming File.idr --exec main`  
  Alternatively you can type `idris2 --repl examples.ipkg` and then `:module File` and finally `:exec main`

* `Network.idr` has an example of connecting to a website and streaming the results to stdout and a file at the same time.  
It is most directly ran by typing `idris2 -p network -p streaming Network.idr --exec main`  
Alternatively you can type `idris2 --repl examples.ipkg` and then `:module Network` and finally `:exec main`

This currently installs `streaming` when built (for my own convenience) comment out the 'prebuild' line in examples.ipkg if you don't want this to happen.

Version
-------

This package follows [Haskell PVP](https://pvp.haskell.org/) which is distinct from [SEMVER](https://semver.org/) in that when examining `1.2.3`, `1.2`  is the Major Version rather than `1`.
