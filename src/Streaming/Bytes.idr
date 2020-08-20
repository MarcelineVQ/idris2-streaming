module Streaming.Bytes

import Streaming
-- import Streaming.Internal

import System.File

import Control.Monad.Trans

{-
This module provides ways of constructing streams of bytes
-}

intCast : Int -> Bits8
intCast r = believe_me r

export
lines : Monad m => Stream (Of Bits8) m a -> Stream (Stream (Of Bits8) m) m a
lines = split (==10)

export
words : Monad m => Stream (Of Bits8) m a -> Stream (Stream (Of Bits8) m) m a
words = split (==32)

export
unlines : Monad m => Stream (Stream (Of Bits8) m) m a -> Stream (Of Bits8) m a
unlines str = intercalates (yield 10) str <* yield 10

export
unwords : Monad m => Stream (Stream (Of Bits8) m) m a -> Stream (Of Bits8) m a
unwords = intercalates (yield 32)

export
charCast : Bits8 -> Char
charCast r = cast {from=Int} (believe_me r)

-- ByteStream : (Type -> Type) -> Type -> Type
-- ByteStream m a = Stream (Of Bits8) m a

-- takeuntill \n or \r, if \r peek for \n next?
-- lines str = Build (\r,eff,step => ?sdffsd)

-- lines str = Build (\r,eff,step => ?sdfsfd)


export
||| A stream of Bits8 as read from a File from its current position, e.g. fgetc
byteFromFile : HasIO io => File -> Stream (Of Bits8) io ()
byteFromFile handle = Build (\r,eff,step => bef handle r eff step)
  where
    bef : File -> (() -> b) -> (eff : io b -> b) -> (step : Of Bits8 b -> b) -> b
    bef file r eff step = eff $ do
      Right c <- fGetChar file
        | Left err => do Prelude.printLn "file read error"
                         pure (r ())
      -- We check here since fEOF doesn't seem to be set by next Step.
      False <- fEOF file
        | True => pure (r ())
      pure $ step (intCast (cast c) :> bef file r eff step)

-- export
-- bytesFromFile : HasIO io => File -> Nat -> Stream (Of Bytes) io ()
-- bytesFromFile file n = mapped toBytes (chunksOf n (byteFromFile file))
--   where
--     toBytes : Stream (Of Bits8) io x -> io (Of Bytes x)
--     toBytes str = first (pack . map cast) <$> toList str
