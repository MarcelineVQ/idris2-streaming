module Streaming.API

import Prelude as P

import public Streaming.Internal as S

import Control.Monad.Trans -- lift

import Data.LazyList

-- import Util

-- slightly more specific version of inspect
export
%inline
next : Monad m => Stream (Of a) m r -> m (Either r (Of a (Stream (Of a) m r)))
next = inspect

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

||| Appends two streams keeping the value of the latter, for a version that
||| keeps both results use <+>
export
append : Monad m => Stream (Of a) m r -> Stream (Of a) m r -> Stream (Of a) m r
append s1 s2 = Build
 (\r,eff,step => streamFold (\_ => streamFold r eff step s2) eff step s1)

export
concats : (Functor f, Monad m) => Stream (Stream f m) m r -> Stream f m r
concats str = Build
  (\r,eff,step => streamFold r eff (streamFold id eff step) str)

export
||| strict (left) fold
foldl : Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m (Of b r)
foldl f acc (Return r) = pure (acc :> r)
foldl f acc (Effect m) = m >>= foldl f acc
foldl f acc (Step (x :> str)) = foldl f (f acc x) str
foldl f acc (Build g) = foldl f acc (streamBuild g)

export
foldr : Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m (Of b r)
foldr f acc = streamFold (\r => pure (acc :> r)) join
               (\(a :> rest) => first (f a) <$> rest)

export
foldr_ : Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> m b
foldr_ f acc = streamFold (\_ => pure acc) join (\(a :> rest) => f a <$> rest)

foldl_ : Monad m => (b -> a -> b) -> b -> Stream (Of a) m r -> m b
foldl_ f acc (Return r) = pure acc
foldl_ f acc (Effect m) = m >>= foldl_ f acc
foldl_ f acc (Step (x :> str)) = foldl_ f (f acc x) str
foldl_ f acc (Build g) = foldl_ f acc (streamBuild g)

export
foldrM : Monad m => (a -> b -> m b) -> m b -> Stream (Of a) m r -> m (Of b r)
foldrM f acc = streamFold (\r => (:> r) <$> acc) join
                (\(a :> rest) => do
                  (b :> r) <- rest
                  (:> r) <$> f a b)

export -- our fold is foldl so we make a dlist
toList : Monad m => Stream (Of a) m r -> m (Of (List a) r)
toList str = first ($ []) <$> foldl (\diff,a,ls => diff (a :: ls)) id str

||| Run an action on each element of a stream and reyield them, this is quite
||| useful for debug priting of stream values as you work on them.
export
chain : Monad m => (a -> m ()) -> Stream (Of a) m r -> Stream (Of a) m r
chain f str = Build (\r,eff,step => streamFold r eff
  (\(x :> y) => eff $ f x *> pure (step (x :> y))) str)

-- export
-- toList_ : Monad m => Stream (Of a) m r -> m (List a)
-- toList_ = fold_ (::) []

export
length : Monad m => Stream (Of a) m r -> m (Of Int r)
length = foldl (\n,_ => 1 + n) 0

export
length' : Monad m => Stream (Of a) m r -> m (Of Nat r)
length' = foldl (\n,_ => S n) Z

export
length_ : Monad m => Stream (Of a) m r -> m Int
length_ = foldl_ (\n,_ => 1 + n) 0

export
length'_ : Monad m => Stream (Of a) m r -> m Nat
length'_ = foldl_ (\n,_ => S n) Z

export
sum : (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum = foldl (+) 0

export
sum_ : (Monad m, Num a) => Stream (Of a) m r -> m a
sum_ = foldl_ (+) 0

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

-- Stream (Of )

-- yields : f a -> Stream (Of a) m r

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
splitsAt : (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt 0 str = Return str
splitsAt n str = case str of
  (Return x) => Return (Return x)
  (Effect x) => Effect (splitsAt n <$> x)
  (Step x) => case n of
    0 => Return (Step x)
    k => Step $ splitsAt (k-1) <$> x
  (Build g) => splitsAt n (streamBuild g) -- :(

-- idk how to 'Build' this. specifically to carry the Nat to the next 'step'
export
splitsAt' : (Monad m, Functor f) => Nat -> Stream f m r -> Stream f m (Stream f m r)
splitsAt' Z str = Return str
splitsAt' n str = case str of
  (Return x) => Return (Return x)
  (Effect x) => Effect (splitsAt' n <$> x)
  (Step x) => case n of
    Z => Return (Step x)
    (S k) => Step $ splitsAt' k <$> x
  (Build g) => splitsAt' n (streamBuild g) -- :(

-- idk how to 'Build' this
export
chunksOf : (Monad m, Functor f) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n (Return x) = Return x
chunksOf n (Effect x) = Effect (chunksOf n <$> x)
chunksOf n (Step x)   = Step (Step (map (map (chunksOf n) . splitsAt (n - 1)) x))
chunksOf n (Build g)  = chunksOf n (streamBuild g) -- :(

export
chunksOf' : (Monad m, Functor f) => Nat -> Stream f m r -> Stream (Stream f m) m r
chunksOf' n (Return x) = Return x
chunksOf' n (Effect x) = Effect (chunksOf' n <$> x)
chunksOf' n (Step x)   = Step (Step (map (map (chunksOf' n) . splitsAt' (n`minus`1)) x))
chunksOf' n (Build g)  = chunksOf' n (streamBuild g) -- :(


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

export
take : (Functor f, Monad m) => Int -> Stream f m r -> Stream f m ()
take 0 str = pure ()
take n str = case str of
  (Return x) => Return ()
  (Effect x) => Effect (map (take n) x)
  (Step x) => Step (map (take (n - 1)) x)
  (Build g) => g (const (Return ())) (effect . map (take n)) (wrap . map (take (n - 1)))

-- dunno how to 'Build' this
export
take' : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m ()
take' Z str = pure ()
take' n@(S k) str = case str of
  (Return x) => Return ()
  (Effect x) => Effect (map (take' n) x)
  (Step x) => Step (map (take' k) x)
  (Build g) => g (const (Return ())) (effect . map (take' n)) (wrap . map (take' k))

export
drop : (Functor f, Monad m) => Int -> Stream f m r -> Stream f m r
drop 0 str = str
drop n str = Build (\r,eff,step => streamFold r eff step (drop (n - 1) str))

export
drop' : (Functor f, Monad m) => Nat -> Stream f m r -> Stream f m r
drop' Z str = str
drop' (S k) str = Build (\r,eff,step => streamFold r eff step (drop' k str))

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
all p str = foldl (\b,x => p x && b) True str

export
print : (HasIO io, Show a, Show r) => Stream (Of a) io r -> io r
print x = streamFold (\r => printLn r *> pure r) join (\(x :> s) => print x *> s) x <* putStr "\n"

-- infixl 9 `for`
export
-- | @for@ replaces each element of a stream with an associated stream. Note that the
-- associated stream may layer any functor.
for : (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for str act = Build (\r,eff,step => streamFold r eff
  (\(a :> rest) => streamFold r eff step (act a *> for str act)) str)

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
repeat : Monad m => a -> Stream (Of a) m r
repeat x = effect (pure (wrap (x :> repeat x)))

export
replicate : Monad m => Int -> a -> Stream (Of a) m ()
replicate n x = take n (repeat {r=()} x)

-- export
-- replicateM : Monad m => Int -> m a -> Stream (Of a) m ()
-- replicateM n x = take n (repeat x)

export
%inline
zip : Monad m => Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of (a,b)) m r
zip = zipWith MkPair

export
iterate : Monad m => (a -> a) -> a -> Stream (Of a) m r
iterate f x = effect $ pure $ wrap $ x :> iterate f (f x)

-- Helper, I seem to use it a lot but haskell 'streaming' doesn't have it, am I
-- doing something wrong?
-- Not quite sure about its execution complexity with the naive rev style
export
rev : Monad m => Stream (Of a) m r -> Stream (Of a) m r
rev str0 = effect $ do
  Right (x :> str) <- inspect str0
    | Left l => pure . pure $ l
  pure $ rev str <* yield x

||| This is quite strict but it also that way in the haskell implementation
||| because Step is strict there and only lazy in the 2nd field of Of, which we
||| use immediately here, so it's fully strict.
export
effects : Monad m => Stream (Of a) m r -> m r
effects = streamFold pure join sndOf
-- effects (Return x) = pure x
-- effects (Effect x) = x >>= effects
-- effects (Step (x :> y)) = effects y -- too strict
-- effects (Build f) = effects (streamBuild f)


-- drained :: Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
export
drained : (Monad m, Monad (t m), Functor (t m), MonadTrans t)
      => t m (Stream (Of a) m r) -> t m r
drained = join . map (lift . effects)

||| strictly take some of a stream and preserve the return type, it's barely
||| worth it given that it's strict on the whole stream
export -- t is hard to deduce for the compiler so we help it along
take'' : Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
take'' n = drained {t=Stream (Of a)} . splitsAt n
