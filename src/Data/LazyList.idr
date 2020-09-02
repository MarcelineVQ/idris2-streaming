module Data.LazyList

-- Foldable, Functor, etc are all too strict for some reason so this can't implement them for use.

public export
data LazyList : Type -> Type where
  Nil : LazyList a
  (::) : a -> Lazy (LazyList a) -> LazyList a

data NonEmpty : LazyList a -> Type where
  IsNonEmpty : NonEmpty (_ :: _)

export
Functor LazyList where
  map f [] = []
  map f (x :: xs) = f x :: map f xs

export
Show a => Show (LazyList a) where
  show [] = "[]"
  show (x :: xs) = show x ++ " :: " ++ show xs

public export
Semigroup (LazyList a) where
  [] <+> ys = ys
  (x :: xs) <+> ys = x :: (xs <+> ys)

public export
Monoid (LazyList a) where
  neutral = []

export
total
head : LazyList a -> Maybe a
head [] = Nothing
head (x :: _) = Just x

export
total
head' : (l : LazyList a) -> NonEmpty l => a
head' (x :: _) = x

export
partial
head'' : LazyList a -> a
head'' [] = idris_crash "head: LazyList was empty"
head'' (x :: _) = x

export
total
tail : LazyList a -> Maybe (LazyList a)
tail [] = Nothing
tail (_ :: xs) = Just xs

export
total
tail' : (l : LazyList a) -> NonEmpty l => LazyList a
tail' (_ :: xs) = xs

export
partial
tail'' : LazyList a -> LazyList a
tail'' [] = idris_crash "tail: LazyList was empty"
tail'' (_ :: xs) = xs

export
Foldable LazyList where
  foldr f z [] = z
  foldr f z (x :: xs) = f x (foldr f z xs)


export
||| Foldable's methods are strict due to needing (a -> b -> b). `map` and the
||| like can stay lazy because they have a constructor guarding their recursion.
||| We don't have that here and must instead be explicitly lazy in our combining
||| function.
foldr' : (a -> Lazy b -> Lazy b) -> b -> LazyList a -> b
foldr' f z [] = z
foldr' f z (x :: xs) = f x (Delay (foldr' f z xs))

export
toList : LazyList a -> List a
toList = foldr' (\x,xs => x :: force xs) []

export -- Not lazy
fromList : List a -> LazyList a
fromList = foldr (\x,xs => x :: xs) []

-- It's weird but the S Z case is necessary otherwise a list like
-- take 2 [1,2,idris_crash "foo"] will crash. I don't know why that should be,
-- given that xs is Lazy.
export
take : Nat -> LazyList a -> LazyList a
take Z _  = []
take _ [] = []
take (S Z) (x :: _) = x :: []
take (S k) (x :: xs) = x :: take k xs

export
take' : Nat -> Stream a -> LazyList a
take' Z (x :: xs) = []
take' (S k) (x :: xs) = x :: take' k xs

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

export
||| yanked from Prelude.Types
unpack : String -> LazyList Char
unpack str = unpack' (prim__cast_IntegerInt (natToInteger (length str)) - 1) str []
  where
    unpack' : Int -> String -> LazyList Char -> LazyList Char
    unpack' pos str acc
        = if pos < 0
             then acc
             else assert_total $ unpack' (pos - 1) str (assert_total (prim__strIndex str pos)::acc)

export
zipWith : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys
zipWith _ _ _ = []

export
%inline
zip : LazyList a -> LazyList b -> LazyList (a,b)
zip = zipWith MkPair
