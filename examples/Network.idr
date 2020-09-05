module Network

import Streaming
import Streaming.Bytes as BS

import System.File
import Network.Socket

import Control.Monad.Trans

import Control.Monad.Either
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
{- Let's read the text from example.com! We're going to do some parsing here to
   find the text we're after but in real life you'd use a proper parsing lib.
   That could be a good next project, streaming to a parsing lib.
   This example is a little more involved to make use of some extra tooling to
   be expanded on later.
   In a real program you'd want to enforce that connecting has been done before
   sending or receiving, consider Control.Linear.Network for your own tests.

   I expect network primitives to eventually provide Bits8 rather than Char as
   they do now, in the mean time we cast to Bits8. This is currently correct to
   do as these are really a 'c char' which is 8 bits.
-}
-------------------------------------------------

%foreign "C:strerror,libc,string.h"
strerror : Int -> String
-- No reason to PrimIO, right? It's just a lookup.

||| A sum Error type for this example, for making EitherT use direct.
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

-- While this is really Maybe Error, writing it as Either Error () plays nicer
-- with the EitherT setup used here.
streamnet : (Monad m, HasIO m) => Socket -> Stream (Of Bits8) m (Either Error ())
streamnet sock = Effect $ do
  let chunkLen = 1024
  Right (res,len) <- recv sock chunkLen
    | Left err => pure $ Return (Left (RecvError err))
  pure $ if len == 0
    then Return (Right ())
    else each (map (cast . cast {to=Int}) . LL.unpack $ res) *>
           if len < chunkLen
              then Return (Right ())
              else streamnet sock

withFile : HasIO io => String -> Mode
        -> (Either Error File -> io b) -> io b
withFile file mode act = do
  f <- first FileError <$> openFile file mode
  res <- act f
  traverse closeFile f
  pure res

withSocket : HasIO io => SocketFamily -> SocketType -> ProtocolNumber
          -> (Either Error Socket -> io b) -> io b
withSocket fam ty proto act = do
  sock <- first SocketError <$> socket fam ty proto
  res <- act sock
  traverse close sock
  pure res

main : IO ()
main = runManaged $ do
    let filename = "out.txt"
        addr = Hostname "www.example.com"
        msg = makeHeader ["GET / HTTP/1.0"
                         ,"Host: www.example.com"
                         ,"User-Agent: idris2-test"]
    res <- runEitherT $ do
      file <- MkEitherT . use . managed $ withFile "out.txt" WriteTruncate
      sock <- MkEitherT . use . managed $ withSocket AF_INET Stream 0
      0 <- lift $ connect sock addr 80
        | err => throwE (ConnectError err)
      lift $ first SendError <$> send sock msg
      lift $ streamnet sock
          &$ maps charCast
          |> S.stdoutChrLn'
      pure () -- helps type inferrence
    either (putStrLn . showError) pure res
  where
    makeHeader : List String -> String
    makeHeader xs = concatMap (++ "\r\n") xs ++ "\r\n"
