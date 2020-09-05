module Streaming.Bytes

import Streaming
-- import Streaming.Internal

import System.File

import Util

import Control.Monad.Trans -- intercalates needs it

{- This module provides ways of constructing streams of bytes (Bits8) and some
   basic manipulations of them.
-}

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
charCast r = cast (cast {to=Int} r)

-- I expect file primitives to eventually provide Bits8 rather than Char as they
-- do now, in the mean time we cast to Bits8. This is currently correct to do as
-- these are really a 'c char' which is 8 bits.
export
||| A stream of Bits8 as read from an open File from its current position, i.e. fgetc
bits8FromFile : HasIO io => File -> Stream (Of Bits8) io ()
bits8FromFile handle = Build (\r,eff,step => bef handle r eff step)
  where
    bef : File -> (() -> b) -> (eff : io b -> b) -> (step : Of Bits8 b -> b) -> b
    bef file r eff step = eff $ do
      Right c <- fGetChar file
        | Left err => do Prelude.printLn "file read error"
                         pure (r ())
      -- We check here since fEOF doesn't seem to be set by next Step.
      False <- fEOF file
        | True => pure (r ())
      pure $ step (cast (cast {to=Int} c) :> bef file r eff step)

export
||| Open a file and read it as a stream of Bits8
bits8FromFile' : HasIO io => String -> Stream (Of Bits8) io (Either FileError ())
bits8FromFile' file0 = effect $ do
    Right f <- openFile file0 Read
      | Left err => pure . pure $ Left err
    pure $ Build (\r,eff,step => bef f r eff step)
  where
    bef : File -> (Either FileError () -> b) -> (eff : io b -> b)
       -> (step : Of Bits8 b -> b) -> b
    bef file r eff step = eff $ do
      Right c <- fGetChar file
        | Left err => do Prelude.printLn "file read error"
                         closeFile file
                         pure (r (Left err))
      -- We check here since fEOF doesn't seem to be set by next Step.
      False <- fEOF file
        | True => closeFile file *> pure (r (Right ()))
      pure $ step (cast (cast {to=Int} c) :> bef file r eff step)

%foreign "C:fputc,libc"
prim_fputc : Int -> FilePtr -> PrimIO Int

fputc : HasIO io => File -> Bits8 -> io (Either FileError ())
fputc (FHandle ptr) b = do
  let c = cast b
  c' <- primIO $ prim_fputc c ptr
  pure $ if c' == c
    then Right ()
    else Left FileWriteError

export
||| A stream of Bits8 as read from a File from its current position, e.g. fgetc
byteToFile : HasIO io => File -> Stream (Of Bits8) io r -> io (Either FileError r)
byteToFile handle = streamFold (pure . Right) join (bef handle)
  where
    bef : File -> Of Bits8 (io (Either FileError r)) -> io (Either FileError r)
    bef file (b :> r) = do
      Right _ <- fputc file b
        | Left err => do Prelude.printLn "file read error"
                         pure (Left err)
      -- We check here since fEOF doesn't seem to be set by next Step.
      False <- fEOF file
        | True => pure (Left FileWriteError)
      r
