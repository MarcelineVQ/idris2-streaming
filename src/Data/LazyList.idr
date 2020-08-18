module Data.LazyList

public export
data LazyList : Type -> Type where
  Nil : LazyList a
  (::) : a -> Lazy (LazyList a) -> LazyList a

export
||| Foldable's methods are strict
foldr : (a -> b -> b) -> b -> LazyList a -> b
foldr f z [] = z
foldr f z (x :: xs) = f x (foldr f z xs)

-- export
-- Foldable LazyList where
  -- foldr = Data.LazyList.foldr

toList : LazyList a -> List a
toList = Data.LazyList.foldr (\x,xs => x :: force xs) []

export
take : Nat -> Stream a -> LazyList a
take Z (x :: xs) = []
take (S k) (x :: xs) = x :: take k xs

repeat : a -> LazyList a
repeat x = x :: repeat x

takeUntil : (a -> Bool) -> Stream a -> LazyList a
takeUntil f (x :: xs) = if f x then [x] else x :: takeUntil f xs

takeBefore : (a -> Bool) -> Stream a -> LazyList a
takeBefore f (x :: xs) = if f x then [] else x :: takeBefore f xs

export -- [2..7] = [2,3,4,5,6,7]
rangeFromTo : (Range a, Ord a) => a -> a -> LazyList a
rangeFromTo x y = if y > x then takeUntil (>= y) (rangeFrom x)
                           else []

export -- [2,4..7] = [2,4,6]
rangeFromThenTo : (Range a, Ord a) => a -> a -> a -> LazyList a
rangeFromThenTo = ?sdd
-- rangeFromThenTo x y z = if y > x then if z > x  then takeBefore (> z) (rangeFrom )
                                                -- else ?rangeFromThenTo_rhs
                                 -- else if x == y then repeat x
                                                -- else takeBefore (< z) ?dsfds

  
fef : LazyList Int
fef = [1,2..5]