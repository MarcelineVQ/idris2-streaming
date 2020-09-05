module Util

-- NB things will need to be removed from here as Idris gains them in the
-- Prelude.

import System.File

infixl 1 <&>
export
(<&>) : Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x

infixr 1 =<<
export
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- For when Lazy is causing type problems and you want to avoid Force/Delay, or
-- force/delay isn't working as I've noticed it sometimes doesn't.
infixr 4 &&|
export
(&&|) : Bool -> Bool -> Bool
(&&|) x y = x && y

export
catMaybes : List (Maybe a) -> List a
catMaybes z = foldr (\m,f => maybe f (\x => (x ::) . f) m) id z []

export
isJust : Maybe a -> Bool
isJust (Just _ ) = True
isJust _ = False

export
unzip : List (a,b) -> (List a, List b)
unzip = foldr (\(x,y),(xs,ys) => (x :: xs, y :: ys)) ([],[])

-- This is hiiiiiideously slow! maybe it's because I'm using it at elab-time
export
unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 [] = ([],[],[])
unzip3 ((x, (y, z)) :: ls) = let (xs,ys,zs) = unzip3 ls
                             in (x :: xs, y:: ys, z :: zs)

export
unless : Applicative f => Bool -> Lazy (f ()) -> f ()
unless b act = if b then pure () else act

public export
monus : Nat -> Nat -> Nat
monus Z _ = Z
monus k Z = k
monus (S k) (S j) = k `monus`j

export
withFile : HasIO io => String -> Mode -> (Either FileError File -> io a) -> io a
withFile file mode act = do res <- openFile file mode
                            a <- act res
                            either (\_ => pure a)
                                   (\f => pure a <* closeFile f) res

export
on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y

infixl 9 ^
export
%foreign "scheme:expt"
(^) : Int -> Int -> Int

infixl 7 .&.
export
%foreign "scheme:bitwise-and"
(.&.) : Int -> Int -> Int

infixl 8 .|.
export
%foreign "scheme:bitwise-ior"
(.|.) : Int -> Int -> Int

%foreign "scheme:bitwise-xor"
export
xor : Int -> Int -> Int

%foreign "scheme:bitwise-not"
export
not : Int -> Int

export
leadingBits : Bits8 -> Int
leadingBits b0 = count (cast b0) 7
  where
    count : Int -> Int -> Int
    count b p = if p >= 0 && b .&. shiftL 1 p > 0
                  then 1 + count b (p - 1)
                  else 0

-- Bits8 is always a valid codepoint
export
bits8ToChar : Bits8 -> Char
bits8ToChar = cast . cast {to=Int}

export
Range Char where
  rangeFromTo x y = cast <$> rangeFromTo {a=Int} (cast x) (cast y)
  rangeFromThenTo x y z
    = cast <$> rangeFromThenTo {a=Int} (cast x) (cast y) (cast z)
  rangeFrom x = cast <$> rangeFrom {a=Int} (cast x)
  rangeFromThen x y = cast <$> rangeFromThen {a=Int} (cast x) (cast y)
