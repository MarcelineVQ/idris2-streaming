module Streaming.Char

import Streaming
-- import Streaming.Internal

import System.File

import Control.Monad.Trans

{-
This module provides ways of constructing streams of bytes
-}

export
lines : Monad m => Stream (Of Char) m a -> Stream (Stream (Of Char) m) m a
lines = split (=='\n')

export
words : Monad m => Stream (Of Char) m a -> Stream (Stream (Of Char) m) m a
words = split (==' ')

export
unlines : Monad m => Stream (Stream (Of Char) m) m a -> Stream (Of Char) m a
unlines str = intercalates (yield '\n') str <* yield '\n'

export
unwords : Monad m => Stream (Stream (Of Char) m) m a -> Stream (Of Char) m a
unwords = intercalates (yield ' ')
