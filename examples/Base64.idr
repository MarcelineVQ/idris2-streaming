module Base64

-- Example of streaming from/to files.

import Streaming
import Streaming.Bytes as B
import Streaming.Char as C
import Streaming.Encoding.UTF8
import Streaming.Encoding.Base64
import Util -- withFile

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Either

import System.File
import Data.Strings

import Data.List as L -- reverse

import Control.Monad.Managed

-------------------------------------------------
-- Streaming Base64 Encoding/Decoding Example

{- Here we read in some test ascii data and see if it encodes and decodes into
   the same data. A further test is done by reading in from urandom to see if
   entirely arbitrary data roundtrips properly.

   This is written in a 'forward' style to follow along with the steps the
   stream takes. One could as easily use . and $ and write this in regular
   'reverse' style Idris/Haskell composition.
-}
-------------------------------------------------

text : String
text = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

text_enc : String
text_enc = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="

-- rfc reference strings
test_str1_ref : String
test_str1_ref = ""
test_str2_ref : String
test_str2_ref = "f"
test_str3_ref : String
test_str3_ref = "fo"
test_str4_ref : String
test_str4_ref = "foo"
test_str5_ref : String
test_str5_ref = "foob"
test_str6_ref : String
test_str6_ref = "fooba"
test_str7_ref : String
test_str7_ref = "foobar"
-- rfc reference encoded-strings
test_str1_enc : String
test_str1_enc = ""
test_str2_enc : String
test_str2_enc = "Zg=="
test_str3_enc : String
test_str3_enc = "Zm8="
test_str4_enc : String
test_str4_enc = "Zm9v"
test_str5_enc : String
test_str5_enc = "Zm9vYg=="
test_str6_enc : String
test_str6_enc = "Zm9vYmE="
test_str7_enc : String
test_str7_enc = "Zm9vYmFy"


fromString' : Monad m => String -> Stream (Of Char) m ()
fromString' str = each''' (unpack str)

toString' : Monad m => Stream (Of Char) m r -> m String
toString' = S.foldr_ strCons ""

-- String is expected to be comprised of Chars from the standardAlphabet for this test
roundtrip : String -> String
roundtrip s = fromString' s
           &$ maps (cast . ord)
           |> encodeBase64 standardAlphabet
           |> decodeBase64 standardAlphabet
           |> maps (chr . cast)
           |> runIdentity . toString'

-- encode and check against our specific rfc reference strings
enc : String -> String -> Bool
enc s1 s2 = fromString' s1
         &$ maps (cast . ord) -- turn to Bits8
         |> encodeBase64 standardAlphabet
         |> maps (chr . cast) -- back to Char
         |> toString'
         |> (== s2) . runIdentity

-- using Managed for fun, it's not really that useful for just one 'withFoo'
randoStr : Stream (Of Bits8) Managed (Either FileError ())
randoStr = effect $ do
  Right rand <- use . managed $ withFile' "/dev/urandom" Read
    | Left err => pure . pure $ Left err
  pure $ byteFromFile {io=Managed} rand *> pure (Right ())

rando : Monad m => Stream (Of Bits8) m r -> m Bool
rando str = str
     &$ take 1000
     |> copy
     |> encodeBase64 standardAlphabet
     |> decodeBase64 standardAlphabet
     |> S.toList -- encoded/decoded data
     |> S.toList -- original data
     |> map (\(enc :> orig) => zipWith (==) enc (fstOf orig)) -- compare them
     |> map (and . map delay) -- check they're all True
     -- This is surely slow since we're acting on the results of a ran Stream,
     -- ideally we'd want to stream the comparison and not convert to list.

export
main : IO ()
main = do
    putStrLn $ "text encodes: " ++ show (enc text text_enc)
    putStrLn $ "text roundtrips: " ++ show (roundtrip text == text)

    putStrLn $ "st1 encodes: " ++ show (enc test_str1_ref test_str1_enc)
    putStrLn $ "st2 encodes: " ++ show (enc test_str2_ref test_str2_enc)
    putStrLn $ "st3 encodes: " ++ show (enc test_str3_ref test_str3_enc)
    putStrLn $ "st4 encodes: " ++ show (enc test_str4_ref test_str4_enc)
    putStrLn $ "st5 encodes: " ++ show (enc test_str5_ref test_str5_enc)
    putStrLn $ "st6 encodes: " ++ show (enc test_str6_ref test_str6_enc)
    putStrLn $ "st7 encodes: " ++ show (enc test_str7_ref test_str7_enc)   

    putStrLn $ "st1 roundtrips: " ++ show (roundtrip test_str1_ref == test_str1_ref)
    putStrLn $ "st2 roundtrips: " ++ show (roundtrip test_str2_ref == test_str2_ref)
    putStrLn $ "st3 roundtrips: " ++ show (roundtrip test_str3_ref == test_str3_ref)
    putStrLn $ "st4 roundtrips: " ++ show (roundtrip test_str4_ref == test_str4_ref)
    putStrLn $ "st5 roundtrips: " ++ show (roundtrip test_str5_ref == test_str5_ref)
    putStrLn $ "st6 roundtrips: " ++ show (roundtrip test_str6_ref == test_str6_ref)
    putStrLn $ "st7 roundtrips: " ++ show (roundtrip test_str7_ref == test_str7_ref)

    -- slowslowslow, but the encoder works and the memory is good
    runManaged $ do
      r <- rando randoStr
      putStrLn $ "/dev/urandom data rountripped: " ++ show r
    pure ()

