module Network

import Streaming
import Streaming.Bytes as BS

import System.File
import Network.Socket

import Control.Monad.Trans

import Control.Monad.EitherT
import Control.Monad.Managed

import Data.LazyList as LL
import Util -- Either instances


-------------------------------------------------
-- Streaming Network Example
--
-- NB This file is a work in progress, I just wanted to get it put out before I
-- forget to. Exercises task is subject to change.
--
-- NB This example has not been tested to make sure it's running in constant
-- space.
--
{- Let's read the text from example.com! We're going to do some parsing here to
   find the text we're after but in real life you'd use a proper parsing lib.
   That could be a good next project, streaming to a parsing lib.
   This example is a little more involved to make use of some extra tooling to
   be expanded on later.
-}
--
-------------------------------------------------

infixl 9 |> -- flip .
(|>) : (a -> b) -> (b -> c) -> a -> c
f |> g = \x => g (f x)

infixl 1 &$ -- flip $
(&$) : a -> (a -> b) -> b
x &$ f = f x

%foreign "C:strerror,libc,string.h"
strerror : Int -> String
-- No reason to PrimIO, right? It's just a lookup.

data Error = SocketError Int
           | ConnectError Int
           | SendError Int
           | RecvError Int
           | FileError FileError

showError : Error -> String
showError (SocketError err) = "Socket error: " ++ strerror err
showError (ConnectError err) = "Connect error: " ++ strerror err
showError (SendError err) = "Send error: " ++ strerror err
showError (RecvError err) = "Recv error: " ++ strerror err
showError (FileError err) = "File error: " ++ show err

streamnet : (Monad m, HasIO m) => Socket -> Stream (Of Bits8) m (Either Error ())
streamnet sock = Effect $ do
  let chunkLen = 1024
  Right (res,len) <- recv sock chunkLen
    | Left err => pure $ Return (Left (RecvError err))
  pure $ if len == 0
    then Return (Right ())
    else each (map (intCast . cast) . LL.unpack $ res) *>
           if len < chunkLen
              then Return (Right ())
              else streamnet sock

withFile : HasIO io => String -> Mode -> (Either Error File -> io b) -> io b
withFile file mode act = do
  f <- first FileError <$> openFile file mode
  res <- act f
  traverse closeFile f
  pure res

withSocket : HasIO io => SocketFamily -> SocketType -> ProtocolNumber -> (Either Error Socket -> io b) -> io b
withSocket fam ty proto act = do
  sock <- first SocketError <$> socket fam ty proto
  res <- act sock
  traverse close sock
  pure res

main : IO ()
main = runManaged $ do
  let filename = "out.txt"
  let addr = Hostname "www.example.com"
  let msg = "GET / HTTP/1.0\r\nHost: www.example.com\r\nUser-Agent: fetch.c\r\n\r\n"
  res <- runEitherT $ do
    file <- MkEitherT . use . managed $ withFile "out.txt" WriteTruncate
    sock <- MkEitherT . use . managed $ withSocket AF_INET Stream 0
    0 <- lift $ connect sock addr 80
      | err => throwE (ConnectError err)
    lift $ first SendError <$> send sock msg
    lift $ S.stdoutChrLn' (maps charCast (streamnet sock))
    pure () -- helps type inferrence
  either (putStrLn . showError) pure res
