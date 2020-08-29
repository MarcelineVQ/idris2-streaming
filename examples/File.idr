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

infixl 9 |> -- flip .
(|>) : (a -> b) -> (b -> c) -> a -> c
f |> g = \x => g (f x)

infixl 1 &$ -- flip $
(&$) : a -> (a -> b) -> b
x &$ f = f x

-------------------------------------------------
-- Streaming File Read Example
{- Here we read a file, split on newlines, split on words, reverse each word,
   unwords, unlines and spit it back out.
   This is written in a 'forward' style to follow along with the steps the
   stream takes. One could as easily use . and $ and write this in regular
   'reverse' style Idris/Haskell composition.

   I expect file primitives to eventually provide Bits8 rather than Char as they
   do now, in the mean time we cast to Bits8. This is currently correct to do as
   these are really a 'c char' which is 8 bits.
-}
-------------------------------------------------

export
main : IO ()
main = do
    let filename = "text.txt"
    Right res <- readFile filename
      | Left err => printLn $ "File error: " ++ filename ++ ", " ++ show err
    putStrLn res
    Right _ <- withFile filename Read $ \f => do
        B.byteFromFile f {io=IO}
          &$ decodeUtf8
          |> encodeUtf8 -- encoding test, temporary
          |> decodeUtf8 -- encoding test, temporary
          |> C.lines
          |> S.mapf (C.words
                  |> S.mapf (S.toList |> map (first reverse) |> S.fromList')
                  |> C.unwords)
          |> C.unlines
          -- We collect the Chars into a String here instead of just piping them
          -- to stdout because idris backend has some issues right now with a
          -- mismatch between a code point Char and 'c char'. Strings don't
          -- exhibit this issue.
          -- |> map (either show show)
          |> S.fold strCons "" >>= putStrLn . fstOf
      | Left err => printLn $ "File error: " ++ filename ++ ", " ++ show err
    pure ()
