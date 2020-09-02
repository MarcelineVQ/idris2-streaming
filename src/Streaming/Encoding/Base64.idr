module Streaming.Encoding.Base64


import Streaming.Internal as S
import Streaming.API as S

import Data.Functor.Of

import Streaming.Bytes

import System.File

import Data.Strings

-- import Data.LazyList
import Data.List -- zip

import Data.IOArray

import Control.Monad.State

import Util

import Language.Reflection



-- base64 is an encoding _from_ binary data to a restricted alphabet of ascii
-- characters. Represented here by Char.

-- utf-8 encodes a unicode codepoint to binary
-- bits64 encodes binary to a restricted ascii alphabet

export
data EncodeError = CodepointOutOfRange
                 | InvalidStartByte Bits8
                 | CodepointEndedEarly 

export
Show EncodeError where
  show CodepointOutOfRange = "CodepointOutOfRange"
  show (InvalidStartByte x) = "InvalidStartByte " ++ show x
  show CodepointEndedEarly = "CodepointEndedEarly"

export
data DecodeError = CharNotInAlphabet Char | DataEndedEarly | TooMuchPadding

export
Show DecodeError where
  show (CharNotInAlphabet x) = "CharNotInAlphabet: " ++ show x
  show DataEndedEarly = "Encoded data ended early"
  show TooMuchPadding = "TooMuchPadding"

data Bits6 : Type where
  B6 : Bits8 -> Bits6

-- Alphabets MUST BE 64 characters, 0-63 not including pad, this is the base64.
record Alphabet where
  constructor MkAlphabet
  toChar : Int -> Char -- to/from the 64
  fromChar : Char -> Maybe Int -- to/from the 64
  pad : Char -- not part of the 64
  whitespace : Char -> Bool -- not part of the 64

-- Can I precompile this into an array?
export
standardAlphabet : Alphabet
standardAlphabet
  = let alph = zip [0..63] $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']
    in MkAlphabet
         (\i => maybe 'A' id (lookup i alph))
         (\c => fst <$> find ((== c) . snd) alph)
         '='
         (`elem` ['\n','_'])

-- base64 can't have a decode error because it pads missing bits.
export
encodeBase64 : Monad m => Alphabet -> Stream (Of Bits8) m r
          -> Stream (Of Char) m r
encodeBase64 alph str0
    = str0 &$
       chunksOf 3 -- this foldl needs to account for folding over 3 instead of less
    |> mapf (store (\str => foldl (\acc,b => shiftL acc 8 .|. cast b) 0
                              (take'' 3 $ str <* replicate 3 0)) -- pad to 3
            |> length -- <^ compute length and combine bits in one pass
            |> \res => effect $ do
                       len :> b <- res
                       pure $ case len of
                         1 => splitGroup 2 b <* yield 65 <* yield 65 -- pads
                         2 => splitGroup 3 b <* yield 65
                         _ => splitGroup 4 b)
    -- ^ Combine up to three Bits8, note the number we got, split based on it
    |> concats
    |> encode alph
  where
    -- Split the Int into 4 six bit parts. Left as Int to skip a cast.
    -- Because we construct the pieces ourself we don't need to do checks later
    -- to be sure our Int fits into our alphabet. Their size means they must.
    splitGroup : forall r. Int -> Of Int r -> Stream (Of Int) m r
    splitGroup n (x :> r) = (iterate (`shiftR`6) x
                         &$ take {r=()} 4
                         |> maps (.&. 0x3F)
                         |> rev -- the shifting makes the order backward
                         |> take n) *> pure r
                         -- e. = 101 46
                         -- Z S 4 = 25 28 56
                         -- A G U u = 0 6 20 46
    -- Take our six bit parts and convert them to chars.
    encode : forall r. Alphabet -> Stream (Of Int) m r -> Stream (Of Char) m r
    encode alph str0 = effect $ do
      Right (b :> str) <- inspect str0
        | Left r => pure . pure $ r
      pure $ if b == 65 -- pad
        then yield alph.pad *> encode alph str
        else yield (alph.toChar b) *> encode alph str

-- What to do if encoded data ends early?
-- Return what was decoded and the rest of the stream?
||| decode Char (from an Alphabet) to binary data
export
decodeBase64 : Monad m => Alphabet -> Stream (Of Char) m r
         -> Stream (Of Bits8) m (Either DecodeError r)
decodeBase64 alph str0 = str0
                      &$ S.filter (not . alph.whitespace) -- ignore whitespace
                      |> validate alph
                      |> chunksOf 4
                      |> mapf (graft alph)
                      |> concats
  where
    -- Ensure the encountered character is part of our alphabet
    validate : forall r. Alphabet -> Stream (Of Char) m r
            -> Stream (Of Char) m (Either DecodeError r)
    validate alph str0 = effect $ do
       Right (c :> str) <- inspect str0
         | Left l => pure . pure . Right $ l
       pure $ if c /= alph.pad
         then case alph.fromChar c of
                Nothing => pure . Left $ CharNotInAlphabet c
                Just _ => wrap (c :> validate alph str)
         else wrap (c :> validate alph str)

    -- Split the collected bytes into `n` eight bit parts.
    splitGroup : forall r. Int -> Of Int r -> Stream (Of Bits8) m r
    splitGroup n (x :> r) = (iterate (`shiftR`8) x
                         &$ take {r=()} 3
                         |> rev
                         |> take n
                         |> maps cast) -- the shifting makes the order backward
                         *> Return r
    -- process 4 chars into 3 bytes
    -- store, check length gotten, unSplit, use what length demands pad otherwise
    graft : forall r. Alphabet -> Stream (Of Char) m r -> Stream (Of Bits8) m r
    graft alph str0 = str0 &$ store (maps (maybe 0 id . alph.fromChar)
                                    |> foldl (\acc,c => shiftL acc 6 .|. c) 0) -- merge chars)
                           |> (length . S.filter (/= alph.pad))
                           |> \x => effect $ do
                             len :> res <- x
                             case len of
                               -- 1 => pure . pure $ Left TooMuchPadding
                               2 => pure (splitGroup 1 res)
                               3 => pure (splitGroup 2 res)
                               _ => pure (splitGroup len res)
-- doesn't quite handle padding right yet
