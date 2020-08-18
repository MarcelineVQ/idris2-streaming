module Data.Functor.Compose

public export
data Compose : (f : Type -> Type) -> (g : Type -> Type) -> Type -> Type where
  MkCompose : f (g x) -> Compose f g x

public export
getCompose : Compose f g a -> f (g a)
getCompose (MkCompose x) = x

public export
(Functor f, Functor g) => Functor (Compose f g) where
  map f (MkCompose x) = MkCompose $ map (map f) x

