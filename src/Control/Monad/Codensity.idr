module Control.Monad.Codensity

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import Control.Linear.LIO

public export
data Codensity : (m : Type -> Type) -> (a : Type) -> Type where
  MkCodensity : (1 _ : (forall b. (a -> m b) -> m b)) -> Codensity m a

runCodensity : Codensity m a -> (forall b. (a -> m b) -> m b)
runCodensity (MkCodensity x) = x

public export
implementation Functor (Codensity m) where
  map f g = MkCodensity $ \bmr => runCodensity g (\a => bmr (f a))

public export
implementation Applicative (Codensity m) where
  pure x = MkCodensity \amr => amr x
  f <*> x = MkCodensity \bmr =>
   runCodensity f (\ab => runCodensity x (\a => bmr (ab a)))

public export
implementation Alternative m => Alternative (Codensity m) where
  x <|> y = MkCodensity \k => runCodensity x k <|> runCodensity y k
  empty = MkCodensity \_ => empty

public export
implementation Monad (Codensity m) where
  x >>= k = MkCodensity \bmr => runCodensity x (\a => runCodensity (k a) bmr)

-- The linearity constraint on liftIO is a little obnoxious. I don't really see
-- the point of it.
public export
implementation LinearIO m => HasIO (Codensity m) where
  liftIO x = MkCodensity \1 act => bindL (liftIO x) act

public export
implementation MonadTrans Codensity where
  lift x = MkCodensity (Prelude.(>>=) x)

MonadReader r m => MonadReader r (Codensity m) where
  ask = MkCodensity (ask >>=)
  local f (MkCodensity co) = MkCodensity \amb => do
    r <- ask
    local f . co $ local (const r) . amb

MonadReader s m => MonadState s (Codensity m) where
  get = MkCodensity (ask >>=)
  put s = MkCodensity \amb => local (const s) (amb ())
