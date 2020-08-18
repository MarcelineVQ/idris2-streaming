module Streaming.Bytes

import Data.Bytes.Strict
import Data.Word.Word8
import Streaming

import System.File

{-
This module provides ways of constructing streams of Bytes
-}

intCast : Int -> Bits8
intCast r = believe_me r

export
charCast : Bits8 -> Char
charCast r = cast {from=Int} (believe_me r)

export
byteFromFile : HasIO io => File -> Stream (Of Bits8) io ()
byteFromFile handle = Build (\r,eff,step => bef handle r eff step)
  where
    bef : File -> (() -> b) -> (eff : io b -> b) -> (step : Of Bits8 b -> b) -> b
    bef file@(FHandle f) r eff step = eff $ do
      False <- fEOF file -- this doesn't seem to be set by the time I call again
        | True => pure (r ())
      Right c <- fGetChar file
        | Left err => do print "file read error"
                         pure (r ())
      False <- fEOF file -- fEOF isn't setting correctly? so we check again
        | True => pure (r ())
      pure $ step (intCast (cast c) :> bef file r eff step)

export
bytesFromFile : HasIO io => File -> Nat -> Stream (Of Bytes) io ()
bytesFromFile file n = mapsM toBytes (chunksOf n (byteFromFile file))
  where
    toBytes : Stream (Of Bits8) io x -> io (Of Bytes x)
    toBytes str = first (pack . map cast) <$> toList str
