module Control.Monad.EitherT

import Util

-- import Data.Functor.Bifunctor

import Control.Monad.Trans

-- Does this meet the requirements for a newtype?
public export
data EitherT : (l : Type) -> (m : Type -> Type) -> (r : Type) -> Type where
  MkEitherT : (1 _ : m (Either l r)) -> EitherT l m r

export
%inline
runEitherT : EitherT l m r -> m (Either l r)
runEitherT (MkEitherT x) = x

export
-- ||| map the underlying computation
mapEitherT : (m (Either l r) -> n (Either l' r')) -> EitherT l m r -> EitherT l' n r'
mapEitherT f = MkEitherT . f . runEitherT

public export
Functor m => Functor (EitherT l m) where
  map f e = MkEitherT $ map f <$> runEitherT e --second f <$> runEitherT e

public export
Applicative m => Applicative (EitherT l m) where
  pure = MkEitherT . pure . Right
  f <*> x = MkEitherT [| runEitherT f <*> runEitherT x |]

public export
Monad m => Monad (EitherT l m) where
  x >>= k = MkEitherT $ runEitherT x >>= either (pure . Left) (runEitherT . k)

export
throwE : Applicative m => e -> EitherT e m a
throwE = MkEitherT . pure . Left

export
catchE : Monad m => EitherT e m a -> (e -> EitherT e' m a) -> EitherT e' m a
catchE et f
  = MkEitherT $ runEitherT et >>= either (runEitherT . f) (pure . Right)

-- We can't have both MonadTrans and Bifunctor, so we'll define Bifunctor-like
-- things here since they're just methods.
bimap : Functor m => (a -> b) -> (c -> d) -> EitherT a m c -> EitherT b m d
bimap f g = MkEitherT . map (either (Left . f) (Right . g)) . runEitherT

||| aka `withExcept`
first : Functor m => (a -> b) -> EitherT a m c -> EitherT b m c
first f = bimap f id

second : Functor m => (c -> d) -> EitherT a m c -> EitherT a m d
second g = bimap id g

public export
MonadTrans (EitherT l) where
  lift = MkEitherT . map Right

public export
HasIO m => HasIO (EitherT l m) where
  liftIO act = MkEitherT $ liftIO (io_bind act (pure . Right))

