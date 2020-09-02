module Data.Functor.Of

import Data.Bifunctor

-- Of exists to allow stream functions to be polymorphic over the choice of
-- stream value. This lets us reuse code for nested streams.
-- Stream (Of a) m r vs Stream (Stream (Of a) m) m r
-- It's being placed here just in case it ends up more useful than just for
-- streams

infixr 5 :>
public export
data Of : Type -> Type -> Type where
  ||| Streaming depends on the Lazyness of r currently
  (:>) : a -> Lazy r -> Of a r

-- Eq
-- Ord

public export
Bifunctor Of where
  bimap f g (x :> y) = (f x :> g y)
  first f (x :> y) = (f x :> y)
  second g (x :> y) = (x :> g y)

public export
Functor (Of a) where
  map f o = second f o

-- Applicative
-- Monad
-- Foldable    -- I'm not sure I want to provide these, they're quite strict
-- Traversable -- I'm not sure I want to provide these, they're quite strict


public export
(Semigroup a, Semigroup r) => Semigroup (Of a r) where
  (a :> c) <+> (b :> d) = (a <+> b) :> (c <+> d)

public export
(Monoid a, Monoid r) => Monoid (Of a r) where
  neutral = neutral :> neutral

public export
(Show a, Show r) => Show (Of a r) where
  showPrec d (x :> y) = showParens (d >= Backtick) (show x ++ " :> " ++ show y)

%inline
export
fstOf : Of a b -> a
fstOf (x :> y) = x

%inline
export
sndOf : Of a b -> b
sndOf (x :> y) = y
