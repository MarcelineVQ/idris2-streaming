module Streaming.Encoding.Base64.Alphabet

import Util -- Char instances

import Data.List

import Data.AVL

import Data.AVL

-- Alphabets MUST BE 64 characters, 0-63 not including pad, this is the base64.
public export
record Alphabet where
  constructor MkAlphabet
  toChar : Int -> Char -- to/from the 64
  fromChar : Char -> Maybe Int -- to/from the 64
  pad : Char -- not part of the 64
  whitespace : Char -> Bool -- not part of the 64

-- An array lookup would be better for toChar
-- ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']
export
standardAlphabet : Alphabet
standardAlphabet
  = let chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']
        arf1 = fromList (zip [0..63] chars) -- basic BST for log(n) lookup
        arf2 = fromList (zip chars [0..63]) -- basic BST for log(n) lookup
    in MkAlphabet
         (\i => maybe 'A' id (lookup i arf1))
         -- (\c => fst <$> find ((== c) . snd) alph)
         (\c => lookup c arf2)
         '='
         (`elem` ['\n','_'])
