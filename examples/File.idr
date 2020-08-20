module File

-- Example of streaming from/to files.

import Streaming
import Streaming.Bytes as BS
import Util -- withFile

import System.File

import Data.List as L -- reverse

infixl 9 |> -- flip .
(|>) : (a -> b) -> (b -> c) -> a -> c
f |> g = \x => g (f x)

infixl 1 &$ -- flip $
(&$) : a -> (a -> b) -> b
x &$ f = f x

{- Here we read a file, split on newlines, split on words, reverse each word,
   unwords, unlines and spit it back out.
   This is written in a 'forward' style to follow along with the steps the
   stream takes. One could as easily use . and $ and write this in regular
   'reverse' style Idris/Haskell composition.
-}
export
main : IO ()
main = do
  let filename = "text.txt"
  Right res <- readFile filename
    | Left err => printLn $ "File error: " ++ filename ++ ", " ++ show err
  putStrLn res
  Right _ <- withFile filename Read $ \f => do
      byteFromFile f
        &$ BS.lines
        |> S.maps (BS.words
                |> S.maps (S.toList |> map (first reverse) |> S.fromList')
                |> BS.unwords)
        |> BS.unlines
        |> mapv charCast -- Just for example display purposes.
        |> stdoutChrLn
    | Left err => printLn $ "File error: " ++ filename ++ ", " ++ show err
  pure ()
