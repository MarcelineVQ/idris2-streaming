module Streaming.Encoding.UTF8

import Streaming.Internal as S
import Streaming.API as S

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

-- check leading bit, use that to 'split off' the number of extra Bits8 needed.
-- Check the split stream to make sure those all have follower bits and .|. them
-- together into a larger type (int) after adjusting their position. If they
-- don't all match then error out, if they do call it a Char.
-- store, conceptually, acts on a copy of the substream we're splitting off
-- so we can check validity and roll up the bits at the same time.
-- messy messy coding, but working, I really need to learn bits better
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
           str &$ splitsAt x'
               |> store ( maps cast -- Move to Int
                       |> cons (maskMarker x' b) -- add lead bit to stream front
                       |> S.foldl collect 0) -- combine stream's bits
                       -- |> map (first snd)) -- extract final Int
               |> S.all ((1 ==) . leadingBits) -- requisit bits matched?
               |> \res => do
                    True :> n :> s <- res
                      | _ => pure . pure . Left $ CodepointEndedEarly
                    pure $ if n < 0 || n > 0x10FFFF
                      then pure . Left $ CodepointOutOfRange
                      else wrap (cast n :> decodeUtf8 s)
  where -- 😀 = \128512 = 0x1F600
    maskMarker : (c : Int) -> Bits8 -> Int
    maskMarker c b = shiftL (shiftR 0xFF (c+3) .&. cast b) (6 * c+1)

    collect : Int -> Int -> Int
    collect acc x = shiftL acc 6 .|. (x .&. 0x3F)

private
%inline
if' : Bool -> a -> a -> a
if' x y z = if x then y else z

-- determine our codepoint, split into requisite pieces, cast, mask, stream
-- phew, how do I clean this up a bit?
private
encode : Monad m => Char -> Stream (Of Bits8) m (Either EncodeError ())
encode c = let ic = ord c
           in  if' (ic <= 0x00007F) (map Right (enc 1 ic))
             $ if' (ic <= 0x0007FF) (map Right (enc 2 ic))
             $ if' (ic <= 0x00FFFF) (map Right (enc 3 ic))
             $ if' (ic <= 0x10FFFF) (map Right (enc 4 ic))
             $ pure (Left EncCodepointOutOfRange)
  where
    -- separate our codepoint into a stream of Bits8 then mask it off
    -- e.g. 32767 is 13 1-bits or 111111111111111
    -- our splitting here shifts the number 6 bits each iteration
    -- step1: 111111111111111   And masked: 10111111
    -- step2: 000000111111111               10111111
    -- step3: 000000000000111               10000111
    shiftedBytes : Int -> Stream (Of Bits8) m r
    shiftedBytes x
      = maps (\b => cast (0x80 .|. (b .&. 0x3F))) (iterate (`shiftR`6) x)
    -- Set up the starting byte that encodes what bytes follow:
    -- The count of leading 1's, terminated by a 0, says how many parts a
    -- codepoint has, e.g. codepoint 128512 is a 4 byte codepoint, it happens
    -- to start with the exact byte 11110000. Conceptually separated as
    -- 1111|0|000, 4 1's to show the count of bytes that make the codepoint,
    -- a terminator bit 0, and the first 3 bits of the codepoint.
    -- I suspect these teminator bits aren't really neccesary and are perhaps
    -- for utf16's use.
    encstart : (n : Int) -> (ic : Int) -> Bits8
    encstart 1 ic = cast ic
    encstart n ic =
      cast $ shiftL 0xFE (8 - (n+1)) -- starting bit mask
         .|. (shiftR 0xFF (n+1) .&. shiftR ic (8 * n)) -- mask and fill bits

    -- Combine our starting byte and the shifted follower bytes.
    enc : (n : Int) -> (ic : Int) -> Stream (Of Bits8) m ()
    enc n ic = encstart n ic
                     `cons` rev (take {r=()} (n - 1) (shiftedBytes ic))

export
encodeUtf8 : Monad m => Stream (Of Char) m r
         -> Stream (Of Bits8) m (Either EncodeError r)
encodeUtf8 str0 = effect $ do
  Right (c :> str) <- inspect str0
    | Left l => pure (pure (Right l))
  pure $ encode c *> encodeUtf8 str
