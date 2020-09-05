module File

-- Example of streaming from/to files.

import Streaming
import Streaming.Bytes as B
import Streaming.Char as C
import Streaming.Encoding.UTF8
import Util -- withFile

import System.File
import Data.Strings

import Data.List as L -- reverse

-------------------------------------------------
-- Streaming File Read Example
{- Here we read a file, split on newlines, split on words, reverse each word,
   unwords, unlines and spit it back out.
   This is written in a 'forward' style to follow along with the steps the
   stream takes. One could as easily use . and $ and write this in regular
   'reverse' style Idris/Haskell composition.
-}
-------------------------------------------------

export
main : IO ()
main = do
    let filename = "text.txt"
    Right res <- readFile filename
      | Left err => fileBad filename err
    putStrLn res
    B.bits8FromFile' filename
      &$ decodeUtf8
      |> encodeUtf8 -- encoding roundtripping test, temporary
      |> decodeUtf8 -- encoding roundtripping test, temporary
      |> C.lines
      |> S.mapf (C.words |> S.mapf S.rev |> C.unwords)
      |> C.unlines
      -- We collect the Chars into a String here instead of just piping them
      -- to stdout because the idris backend for printing has some issues
      -- right now with non-ascii Chars. Strings don't exhibit this issue.
      -- More directly single-Char printing uses putchar which on the c-side
      -- casts from int to char which is lossy.
      |> S.foldr strCons "" >>= putStrLn . fstOf
    pure ()
  where
    fileBad : String -> FileError -> IO ()
    fileBad fname err = printLn $ "File error: " ++ fname ++ ", " ++ show err
