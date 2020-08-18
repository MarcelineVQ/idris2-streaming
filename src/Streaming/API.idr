module Streaming.API

import Prelude as P

import public Streaming.Internal as S

import Data.LazyList

import Util

export
%inline
cons : Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
cons x str = wrap (x :> str)
-- cons x str = Build (\r,eff,step => step (x :> streamFold r eff step str))

export
append : Monad m => Stream (Of a) m r -> Stream (Of a) m r -> Stream (Of a) m r
append s1 s2 = Build (\r,eff,step => streamFold r eff (\(x :> y) => (streamFold r eff step s2)) s1)

-- append : (Monad m) => Stream (Of a) m a -> Stream f m a -> Stream f m a
-- append s1 s2 = streamFold ?append_rhs ?sdd ?dsfds s1

export
concats : (Functor f, Monad m) => Stream (Stream f m) m r -> Stream f m r
concats str = Build (\r,eff,step => streamFold r eff (streamFold id eff step) str)

-- dunno how to 'Build' this
export
take : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m ()
take Z str = pure ()
take n@(S k) str = case str of
  (Return x) => Return ()
  (Effect x) => Effect (map (take n) x)
  (Step x) => Step (map (take k) x)
  (Build g) => g (const (Return ())) (effect . map (take n)) (wrap . map (take k))

-- take' : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m ()
-- take' Z str = Build (\r,eff,step => r ())
-- take' (S n) str = Build (\r,eff,step => streamFold (const (r ()))
--   (\z => streamFold r eff ?sdeff (take' n str))
--   ?Sdf
--   str)

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

-- 'Each' of these could simply be Foldable if Foldable's foldr wasn't so strict

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
each'' = each . arf
  where
    arf : List a -> LazyList a
    arf [] = []
    arf (x :: xs) = x :: arf xs

export
||| You are best to avoid this, foldable's methods are strict, your source will
||| reside in memory all at once. And (for some reason) in a manner worse than
||| plain List.
each''' : (Monad m, Foldable f) => f a -> Stream (Of a) m ()
each''' = foldr cons empty

-------------------------------------------------
-- Splitting Streams
-------------------------------------------------

-- idk how to 'Build' this
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

-- idk how to 'Build' this
export
chunksOf : (Monad m, Functor f) => Nat -> Stream f m r -> Stream (Stream f m) m r
chunksOf n (Return x) = Return x
chunksOf n (Effect x) = Effect (chunksOf n <$> x)
chunksOf n (Step x)   = Step (Step (map (map (chunksOf n) . splitsAt (n`monus`1)) x))
chunksOf n (Build g)  = chunksOf n (streamBuild g) -- :(



export
print : (HasIO io, Show a) => Stream (Of a) io r -> io r
print x = streamFold pure join (\(x :> s) => printLn x *> s) x

-- main = S.print (S.mapM readIORef (S.mapM newIORef (S.each [1..10^8::Int])))