module Control.Monad.Managed

import Control.Monad.Trans
import Control.Monad.State

import Control.Monad.EitherT

-------------------------------------------------
{-

Given some:
example : MonadManaged m => EitherT e m a

Your typical use will likely end up looking like this:
main = runManaged $ do
    Right x <- runEitherT example
      | Left y => ...
    ...
  
-}
-------------------------------------------------

export
data Managed : Type -> Type where
  MkManaged : (1 _ : forall b. (a -> IO b) -> IO b) -> Managed a

getManaged : Managed a -> (forall b. (a -> IO b) -> IO b)
getManaged (MkManaged g) = g

export
%inline
managed : (forall r. (a -> IO r) -> IO r) -> Managed a
managed = MkManaged

export
managed_ : (forall r. IO r -> IO r) -> Managed ()
managed_ f = managed $ \g => f (g ())

export
||| This can let you leak the resource through r, be very wary!
||| Best to avoid using.
manageWith : Managed a -> (a -> IO r) -> IO r
manageWith m = getManaged m

export
runManaged : Managed () -> IO ()
runManaged (MkManaged g) = g pure

public export
implementation Functor Managed where
  map f g = MkManaged $ \bmr => getManaged g (\a => bmr (f a))

public export
implementation Applicative Managed where
  pure x = MkManaged (\amr => amr x)
  f <*> x = MkManaged $ \bmr =>
   getManaged f (\ab => getManaged x (\a => bmr (ab a)))

public export
implementation Monad Managed where
  x >>= k = MkManaged $ \bmr => getManaged x (\a => getManaged (k a) bmr)

-- The linearity constraint on HasIO is pretty obnoxious. I don't really see the
-- point of it and I don't want to require LinearIO of my users. Do I need a
-- MonadUnliftIO/MonadBase or something to bring my HasIO down to IO?
-- NB Just gonna go with IO for now.
public export
implementation HasIO Managed where
  liftIO io = MkManaged $ io_bind io

public export
interface HasIO m => MonadManaged (m : Type -> Type) where
  use : Managed a -> m a -- bad name

public export
MonadManaged Managed where
  use = id

public export
MonadManaged m => MonadManaged (StateT s m) where
  use x = lift (use x)
