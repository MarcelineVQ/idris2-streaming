module Data.Functor.Bifunctor

public export
interface Bifunctor (p : Type -> Type -> Type) where
  %inline
  bimap : (a -> b) -> (c -> d) -> p a c -> p b d
  %inline
  first : (a -> b) -> p a c -> p b c
  first f = bimap f id
  %inline
  second : (c -> d) -> p a c -> p a d
  second g = bimap id g

bimapM : (Bifunctor p, Monad m) => (a -> m b) -> (c -> m d) -> p a b -> m (p b d)
-- bimapM f g x = ?bimapM_rhs

firstM : (Bifunctor p, Functor m) => (a -> m b) -> p a c -> m (p b d)
-- firstM f x = bimapM ?sdfd ?sdf (first f x)

-- firstM : (a -> m b) -> p a c -> m (p b d)

public export
Bifunctor Pair where
  bimap f g (x,y) = (f x, g y)
  first f (x,y) = (f x, y)
  second f (x,y) = (x, f y)

public export
Bifunctor Either where
  bimap f g x = either (Left . f) (Right . g) x
