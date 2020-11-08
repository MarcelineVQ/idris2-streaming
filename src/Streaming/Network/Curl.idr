module Streaming.Network.Curl

import Streaming

import Data.LazyList as L

import Data.IORef

import Data.Buffer

import Control.Monad.Trans
import Control.Monad.Managed
import Network.Curl.Easy

import Streaming.API as S
import Streaming.Encoding.UTF8

-- This is just an experiment right now, don't take it as the right way to go

-- ioref seems good still
streamCallback : IORef (LazyList String) -> (Buffer -> Int -> Int -> AnyPtr -> PrimIO Int)
streamCallback ref str s len _ = toPrim $ do
  got <- rawSize str
  gr <- map (cast {to=Bits8}) <$> bufferData str
  xs :> Right _ <- S.toList $ decodeUtf8 (each'' gr)
    | _ => pure 0
  modifyIORef ref (pack xs ::)
  pure got

mGlobalInit : Managed CurlECode
mGlobalInit = managed $ \f => do
  ctx <- curl_global_init
  r <- f ctx
  curl_global_cleanup
  pure r

mEasyInit : Managed (Maybe (CurlHandle Easy))
mEasyInit = managed $ \f => do
  h <- curlEasyInit
  case h of
    Nothing => f h
    Just h' => do r <- f h
                  curlEasyCleanup h'
                  pure r

-- I need to set my callback and then call perform
export
curl : String -> Stream (Of Bits8) Managed (Either String ())
curl url = effect $ do
    CURLE_OK <- mGlobalInit
      | r => pure (pure (Left $ "global init not ok: " ++ show r))
    Just h <- mEasyInit
      | Nothing => pure (pure (Left $ "easy init not ok"))
    CURLE_OK <- curlEasySetopt h CURLOPT_URL url
      | r => pure (pure (Left $ "setopt not ok: " ++ show r))
    CURLE_OK <- curlEasySetopt h CURLOPT_URL url
      | r => pure (pure (Left $ "setopt not ok: " ++ show r))
    -- easy_to_bytes h
    ref <- newIORef {a=LazyList String} []
    CURLE_OK <- curlEasySetopt h CURLOPT_WRITEFUNCTION (streamCallback ref)
      | r => pure (pure (Left $ "setopt not ok: " ++ show r))
    CURLE_OK <- curlEasyPerform h
      | r => pure (pure (Left $ "perform not ok: " ++ show r))
    dat <- readIORef ref
    pure $ the (Stream (Of Bits8) Managed (Either String ()))
         $ each dat
        &$ mapf (\(x :> r) => each {m=Managed} (unpack x) *> pure r)
        |> concats
        |> maps (cast . cast {to=Int})
        |> (*> pure (Right ()))


