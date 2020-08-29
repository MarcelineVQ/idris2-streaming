module Streaming.API

import Prelude as P

import public Streaming.Internal as S

import Control.Monad.Trans -- lift

import Data.LazyList

-- import Util

export
%inline
cons : Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
cons x str = wrap (x :> str)
-- cons x str = Build (\r,eff,step => step (x :> streamFold r eff step str))

export
copy : Monad m => Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
copy str = case str of
  (Return x) => Effect . pure . Return $ x
  (Effect x) => Effect . pure . Effect $ copy <$> lift x -- tricky
  (Step (x :> y)) => Effect . pure . Effect $ (Step (x :> Return (Step (x :> copy y))))
  (Build g)  => copy (streamBuild g)

export
store : Monad m => (Stream (Of a) (Stream (Of a) m) r -> t)
     -> Stream (Of a) m r -> t
store f str = f (copy str)

-- Conceptually simple, just run stream1 then run stream2.
export
append : Monad m => Stream (Of a) m r -> Stream (Of a) m r -> Stream (Of a) m r
append s1 s2 = Build
 (\r,eff,step => streamFold (\_ => streamFold r eff step s2) eff step s1)

export
concats : (Functor f, Monad m) => Stream (Stream f m) m r -> Stream f m r
concats str = Build
  (\r,eff,step => streamFold r eff (streamFold id eff step) str)

export
fold : Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m (Of b r)
fold f acc = streamFold (\r => pure (acc :> r)) join
               (\(a :> rest) => first (f a) <$> rest)

export
fold_ : Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m b
fold_ f acc = streamFold (\_ => pure acc) join (\(a :> rest) => f a <$> rest)

export
toList : Monad m => Stream (Of a) m r -> m (Of (List a) r)
toList = fold (::) []

export
toList_ : Monad m => Stream (Of a) m r -> m (List a)
toList_ = fold_ (::) []

export
length : Monad m => Stream (Of a) m r -> m (Of Int r)
length = fold (\_,n => 1 + n) 0

export
length_ : Monad m => Stream (Of a) m r -> m Int
length_ = fold_ (\_,n => 1 + n) 0


export
sum : (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum = fold (+) 0

export
sum_ : (Monad m, Num a) => Stream (Of a) m r -> m a
sum_ = fold_ (+) 0

-------------------------------------------------
-- Constructing Streams
-------------------------------------------------

public export -- Might as well be public, it's just constructors.
%inline
||| The empty stream, mostly for convenience.
empty : Stream (Of a) m ()
empty = Return ()

export
||| aka 'single', a stream of one element.
yield : Monad m => a -> Stream (Of a) m ()
yield x = cons x empty

-- 'each' of these could simply be Foldable if Foldable's foldr wasn't so strict

export
||| Preferred method of providing List input to a Stream.
each : Monad m => LazyList a -> Stream (Of a) m ()
each [] = empty
each (x :: xs) = cons x (each xs)

export
||| Turn a (list) stream into our Stream.
each' : Monad m => Stream a -> Stream (Of a) m ()
each' (x :: xs) = cons x (each' xs)

export
||| It's better to use LazyList. List will reside in memory all at once.
each'' : Monad m => List a -> Stream (Of a) m ()
each'' = each . fromList

export
||| You are best to avoid this, foldable's methods are strict, your source will
||| reside in memory all at once. And (for some reason) in a manner worse than
||| plain List.
each''' : (Monad m, Foldable f) => f a -> Stream (Of a) m ()
each''' = foldr cons empty

-------------------------------------------------
-- Splitting Streams
-------------------------------------------------

-- idk how to 'Build' this. specifically to carry the Nat to the next 'step'
export
splitsAt : (Monad m, Functor f) => Nat -> Stream f m r -> Stream f m (Stream f m r) 
splitsAt Z str = Return str
splitsAt n str = case str of
  (Return x) => Return (Return x)
  (Effect x) => Effect (splitsAt n <$> x)
  (Step x) => case n of
    Z => Return (Step x)
    (S k) => Step $ splitsAt k <$> x
  (Build g) => splitsAt n (streamBuild g) -- :(

-- idk how to 'Build' this. specifically to carry the Nat to the next 'step'
export
splitsAt' : (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r) 
splitsAt' 0 str = Return str
splitsAt' n str = case str of
  (Return x) => Return (Return x)
  (Effect x) => Effect (splitsAt' n <$> x)
  (Step x) => case n of
    0 => Return (Step x)
    k => Step $ splitsAt' (k-1) <$> x
  (Build g) => splitsAt' n (streamBuild g) -- :(

-- idk how to 'Build' this
export
chunksOf : (Monad m, Functor f) => Nat -> Stream f m r -> Stream (Stream f m) m r
chunksOf n (Return x) = Return x
chunksOf n (Effect x) = Effect (chunksOf n <$> x)
chunksOf n (Step x)   = Step (Step (map (map (chunksOf n) . splitsAt (n`minus`1)) x))
chunksOf n (Build g)  = chunksOf n (streamBuild g) -- :(

-- I was able to 'Build' this because it doesn't need to carry state per
-- invocation, it's a constant predicate.
||| `span` streams elements until one fails the condition, returning the remainder
||| span (<3) [1,2,3,4,5] => [3,4,5]
export
span : Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
span p str = Build (\r,eff,step => streamFold (r . Return) eff (spanIt p r eff step) str)
  where
    spanIt : (a -> Bool) -> (Stream (Of a) m r -> b) -> (m b -> b) -> (Of a b -> b) -> Of a b -> b
    spanIt pred r eff step v@(x :> y) = if pred x
      then step v
      else r $ Step (x :> str)

-- dunno how to 'Build' this
export
take : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m ()
take Z str = pure ()
take n@(S k) str = case str of
  (Return x) => Return ()
  (Effect x) => Effect (map (take n) x)
  (Step x) => Step (map (take k) x)
  (Build g) => g (const (Return ())) (effect . map (take n)) (wrap . map (take k))

export
take' : (Functor f, Monad m) => Int -> Stream f m r -> Stream f m ()
take' 0 str = pure ()
take' n str = case str of
  (Return x) => Return ()
  (Effect x) => Effect (map (take' n) x)
  (Step x) => Step (map (take' (n - 1)) x)
  (Build g) => g (const (Return ())) (effect . map (take' n)) (wrap . map (take' (n - 1)))

export
drop : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m r
drop Z str = str
drop (S k) str = Build (\r,eff,step => streamFold r eff step (drop k str))

export
||| `split` aka splitOn/splitBy
||| splits a stream into segments based on a predicate, consumes True results.
split : Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
split f (Return y) = Return y
split f (Effect y) = Effect (map (split f) y)
split f (Step (a :> rest)) = if not (f a)
  then Step (Prelude.map (split f) (yield a *> span (not . f) rest))
  else split f rest
split f (Build g) = split f (streamBuild g)

export
filter : Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter p str = Build (\r,eff,step => streamFold r eff (\(x :> y) => if p x then step (x :> y) else y) str)

export
all : Monad m => (a -> Bool) -> Stream (Of a) m r -> m (Of Bool r)
all p str = fold (\x,b => p x && b) True str

export
print : (HasIO io, Show a, Show r) => Stream (Of a) io r -> io r
print x = streamFold (\r => printLn r *> pure r) join (\(x :> s) => print x *> s) x <* putStr "\n"


export
concat : (Foldable f, Monad m) => Stream (Of (f a)) m r -> Stream (Of a) m r
concat str = for str each'''

-- Temporary, this seems like a common pattern somehow, I need to find what
-- fundamental operation subsumes it. a variant of how concat is written perhaps
export
fromList : (Foldable f, Monad m) => m (Of (LazyList a) r) -> Stream (Of a) m r
fromList xs = Effect $ do
  (x :> y) <- xs
  let g = each {m} x
  pure (Build (\r,eff,step => streamFold (\_ => r y) eff step g))
  
export
fromList' : (Foldable f, Monad m) => m (Of (f a) r) -> Stream (Of a) m r
fromList' xs = Effect $ do
  (x :> y) <- xs
  let g = each''' {m} x
  pure (Build (\r,eff,step => streamFold (\_ => r y) eff step g))

export
zipWith : Monad m => (a -> b -> c) -> Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of c) m r
zipWith f (Return r) str2 = Return r
zipWith f (Effect m) str2 = Effect $ map (\str => zipWith f str str2) m
zipWith f s@(Step (a :> y1)) str2 = case str2 of
  (Return r) => Return r
  (Effect m) => Effect $ map (zipWith f s) m
  (Step (b :> y2)) => Step $ f a b :> zipWith f y1 y2
  (Build g) => g Return Effect (\(b :> y2) => Step $ f a b :> y2)
zipWith f (Build g) str2 = zipWith f (streamBuild g) str2


export
%inline
zip : Monad m => Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of (a,b)) m r
zip = zipWith MkPair

export
iterate : Monad m => (a -> a) -> a -> Stream (Of a) m r
iterate f x = effect $ pure $ wrap $ x :> iterate f (f x)

-- export
-- reverse : Monad m => Stream (Of a) m r -> Stream (Of a) m r
-- reverse (Return x) = Return x
-- reverse (Effect x) = ?reverse_rhs_2
-- reverse (Step (x :> y)) = reverse y `append` (x `cons` empty)
-- reverse (Build f) = ?reverse_rhs_4