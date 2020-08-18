module Streaming.Streams

import Streaming.Internal
import Streaming.API

import Data.Functor.Of

import System.File

import Data.Strings

{-
The intent of this module is to provide common stream sources, it's not very
thought-through at the moment.
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
        | Left err => do Prelude.print "file read error"
                         r <$> pclose file
      pure $ step (c :> bef file r eff step)

foobaloo1 : IO ()
foobaloo1 = do
  z <- the (IO (Of Int ())) (sum $ map cast $ take 3 stdinLn)
  printLn z
  

foobaloo2 : IO ()
foobaloo2 = stdoutLn $ map toUpper $ take 2 stdinLn

foobaloo3 : IO ()
foobaloo3 = stdoutLn $ map toUpper $ stdinLn