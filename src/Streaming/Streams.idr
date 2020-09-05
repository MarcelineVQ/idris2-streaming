module Streaming.Streams

import Streaming.Internal as S
import Streaming.API as S

import Data.Functor.Of

import System.File

import Data.Strings

import Util

export
data DecodeError = CodepointOutOfRange
                 | InvalidStartByte Bits8
                 | CodepointEndedEarly 

export
Show DecodeError where
  show CodepointOutOfRange = "CodepointOutOfRange"
  show (InvalidStartByte x) = "InvalidStartByte " ++ show x
  show CodepointEndedEarly = "CodepointEndedEarly"

export
data EncodeError = EncCodepointOutOfRange
-- data EncodeError = CodepointOutOfRange
--                  | InvalidStartByte Bits8
--                  | CodepointEndedEarly 
-- 
-- export
export
Show EncodeError where
  show EncCodepointOutOfRange = "CodepointOutOfRange"
--   show (InvalidStartByte x) = "InvalidStartByte " ++ show x
--   show CodepointEndedEarly = "CodepointEndedEarly"

{-
The intent of this module is to provide common stream sources, it's not very
thought-through at the moment and is mostly used as a testbed for things that
move elsewhere.
-}

export
stdinLn : HasIO io => Stream (Of String) io ()
stdinLn = effect $ do
   False <- fEOF stdin
     | True => pure (Return ())
   x <- getLine
   pure $ wrap (x :> stdinLn)

-- readFile : HasIO io => 

-- stdinLn : HasIO io => Stream (Of String) io ()
-- stdinLn = ?sdff
  -- streamFold pure join (\(s :> act) => putStrLn s *> act) str
  -- fflush stdout

export
stdoutLn : HasIO io => Stream (Of String) io () -> io ()
stdoutLn str = do
  streamFold pure join (\(s :> act) => putStrLn s *> act) str
  fflush stdout

export
stdoutChr : HasIO io => Stream (Of Char) io () -> io ()
stdoutChr str = do
  streamFold pure join (\(s :> act) => putChar s *> act) str
  fflush stdout

export
stdoutChrLn : HasIO io => Stream (Of Char) io () -> io ()
stdoutChrLn str = do
  streamFold pure join (\(s :> act) => putChar s *> act) str
  putChar '\n'
  fflush stdout

export
stdoutChrLn' : HasIO io => Stream (Of Char) io r -> io r
stdoutChrLn' str = do
  r <- streamFold pure join (\(s :> act) => putChar s *> act) str
  putChar '\n'
  fflush stdout
  pure r

{-

>>> S.sum $ S.take 3 (S.readLn :: Stream (Of Int) IO ())
1<Enter>
2<Enter>
3<Enter>
6 :> ()

>>> S.stdoutLn $ S.map (map toUpper) $ S.take 2 S.stdinLn
hello<Enter>
HELLO
world!<Enter>
WORLD!

-}


ofHandle : File -> Stream (Of Char) IO ()
ofHandle handle = Build (\r,eff,step => bef handle r eff step)
  where
    bef : File -> (() -> b) -> (eff : IO b -> b) -> (step : Of Char b -> b) -> b
    bef file r eff step = eff $ do
      False <- fEOF file
        | True => r <$> pclose file
      Right c <- fGetChar file
        | Left err => do Prelude.putStrLn "file read error"
                         r <$> pclose file
      pure $ step (c :> bef file r eff step)

foobaloo1 : IO ()
foobaloo1 = do
  z <- the (IO (Of Int ())) (sum $ maps cast $ take 3 stdinLn)
  printLn z
  

foobaloo2 : IO ()
foobaloo2 = stdoutLn $ maps toUpper $ take 2 stdinLn

foobaloo3 : IO ()
foobaloo3 = stdoutLn $ maps toUpper $ stdinLn
