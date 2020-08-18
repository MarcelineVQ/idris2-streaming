Effectful Streaming For Idris
=====

This package provides a central idea and tool for effectful streaming. Heavily based on the [Haskell library](https://hackage.haskell.org/package/streaming) of the same idea.

This isn't a Total streaming library and doesn't make use of more advanced idris features. Totality would be great but unlikely since it imposes restrictions of the Functor's/Monad's we're able to make use of.

On top of general streaming, this package's intent is to provide a basis to stream bytes from sources like files or network. To that end there is a Streaming.Bytes module intended to be used with the [bytes](https://github.com/MarcelineVQ/idris2-bytes) package but some Bits8 methods will be available as well.

TODO
----
Scrutinize uses of inlining to see if it matters at all when we don't have optimization anyway. Further, once we're far enough along: re-implement core forms as just ADTs instead of the Builder pattern and compare the performance. It would have been easier to start that way and tried Builder afterwards but oh well.

Version
-------

This package follows [Haskell PVP](https://pvp.haskell.org/) which is distinct from [SEMVER](https://semver.org/) in that when examining `1.2.3`, `1.2`  is the Major Version rather than `1`.
