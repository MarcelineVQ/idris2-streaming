module Streaming.Encoding.UTF8

import Streaming.Internal as S
import Streaming.API as S

import Data.Functor.Of

import System.File

import Data.Strings

import Util

export
data DecodeError = CodepointOutOfRange
                 | InvalidStartByte Bits8
                 | CodepointEndedEarly 

export
Show DecodeError where
  show CodepointOutOfRange = "CodepointOutOfRange"
  show (InvalidStartByte x) = "InvalidStartByte " ++ show x
  show CodepointEndedEarly = "CodepointEndedEarly"

export
data EncodeError = EncCodepointOutOfRange

export
Show EncodeError where
  show EncCodepointOutOfRange = "CodepointOutOfRange"

infixl 9 |> -- flip .
(|>) : (a -> b) -> (b -> c) -> a -> c
f |> g = \x => g (f x)

infixl 1 &$ -- flip $
(&$) : a -> (a -> b) -> b
x &$ f = f x

-- check leading bit, use that to 'split off' the number of extra Bits8 needed.
-- Check the split stream to make sure those all have follower bits and .|. them
-- together into a larger type (int) after adjusting their position. If they
-- don't all match then error out, if they do call it a Char.
-- store, conceptually, acting on a copy of the substream we're splitting off
-- so we can check validity and roll up the bits at the same time.

-- messy messy, but working
export
decodeUtf8 : Monad m => Stream (Of Bits8) m r
          -> Stream (Of Char) m (Either DecodeError r)
decodeUtf8 str0 = effect $ do
    Right (b :> str) <- inspect str0
      | Left l => pure . pure . pure $ l -- so pure
    case min 4 (leadingBits b) of
      0 => pure $ wrap (bits8ToChar b :> decodeUtf8 str) -- need Delay?
      1 => pure . pure . Left $ InvalidStartByte b
      x => let x' = x - 1 in
           str &$ splitsAt' x'
               |> store ( maps cast -- Move to Int
                       |> cons (maskMarker x' b) -- add lead bit to stream front
                       |> S.fold collect (0,0) -- combine stream's bits
                       |> map (first snd)) -- extract final Int
               |> S.all ((1 ==) . leadingBits) -- requisit bits matched?
               |> \res => do
                    True :> n :> s <- res
                      | _ => pure . pure . Left $ CodepointEndedEarly
                    pure $ if n < 0 || n > 0x10FFFF
                      then pure . Left $ CodepointOutOfRange
                      else wrap (cast n :> decodeUtf8 s)
  where -- 😀 = \128512 = 0x1F600
    maskFollower : Int -> Int
    maskFollower b = b .&. 0x3F

    maskMarker : (c : Int) -> Bits8 -> Int
    maskMarker c b = shiftL (shiftR 0xFF (c+3) .&. cast b) (6 * c+1)

    collect : Int -> (Int, Int) -> (Int, Int)
    collect x (n,acc) = (n+1, acc .|. shiftL (maskFollower x) (6*n))

-- determine our codepoint, split into requisite pieces, cast, mask, stream
-- phew, how do I clean this up?
private
encode : Monad m => Char -> Stream (Of Bits8) m (Either EncodeError ())
encode c = let ic = ord c
           in     if ic <= 0x00007F then map Right $ enc 1 1 ic
             else if ic <= 0x0007FF then map Right $ enc 2 3 ic
             else if ic <= 0x00FFFF then map Right $ enc 3 4 ic
             else if ic <= 0x10FFFF then map Right $ enc 4 5 ic
               else pure (Left EncCodepointOutOfRange)
  where
    shiftedBytes : Int -> Stream (Of Bits8) m r
    shiftedBytes x
      = maps (\b => cast (0x80 .|. (b .&. 0x3F))) (iterate (`shiftR`6) x)

    -- O(n) space, we only ever take max 3 though.
    -- Not quite sure about its execution complexity with the naive rev style
    rev : Stream (Of Bits8) m () -> Stream (Of Bits8) m ()
    rev str0 = effect $ do
      Right (x :> str) <- inspect str0
        | Left l => pure . pure $ l
      pure $ rev str `append` yield x

    encstart : (n : Int) -> (off : Int) -> (ic : Int) -> Bits8
    encstart 1 off ic = cast ic
    encstart n off ic =
      cast $ shiftL 0xFE (8 - off) -- starting bit mask
         .|. (shiftR 0xFF off .&. shiftR ic (8 * n)) -- mask and fill bits

    enc : (n : Int) -> (off : Int) -> (ic : Int) -> Stream (Of Bits8) m ()
    enc n off ic = encstart n off ic
                     `cons` rev (take' {r=()} (n - 1) (shiftedBytes ic))

export
encodeUtf8 : Monad m => Stream (Of Char) m r
         -> Stream (Of Bits8) m (Either EncodeError r)
encodeUtf8 str0 = effect $ do
  Right (c :> str) <- inspect str0
    | Left l => pure (pure (Right l))
  pure $ encode c *> encodeUtf8 str
