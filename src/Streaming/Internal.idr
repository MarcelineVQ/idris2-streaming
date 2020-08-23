module Streaming.Internal

import Control.Monad.Trans

import Data.Functor.Compose
import Data.Functor.Of
import Data.Functor.Bifunctor

import public Data.Functor.Of
import public Data.Functor.Bifunctor
import public Data.Functor.Compose

import Control.Monad.Managed

||| A neat bonus to the Build constructor is that we don't have to 'case' as
||| often when writing our functions because we can just farm out to Build which
||| streamFold will eventually case for us.
||| The Linearities are to satisfy HasIO, they might not stay.
public export
data Stream : (f : Type -> Type) -> (m : Type -> Type) -> Type -> Type where
  Return : (1 _ : r) -> Stream f m r
  Effect : (1 _ : m (Stream f m r)) -> Stream f m r
  Step : (1 _ : f (Stream f m r)) -> Stream f m r
  ||| Fusion constructor
  ||| We don't have a serious rewrite system in idris2 yet so this does the job
  ||| of fusing as long as we're careful about using streamFold and Build
  ||| whenever we can.
  Build : (1 _ : (forall b. (r -> b) -> (m b -> b) -> (f b -> b) -> b)) -> Stream f m r

%name Streaming.Internal.Stream str,str2,str3

export
streamBuild : (forall b . (r -> b) -> (m b -> b) -> (f b -> b) -> b)
           -> Stream f m r
streamBuild = \phi => phi Return Effect Step

export
streamFold : (Functor f, Monad m)
          => (r -> b) -> (m b -> b) -> (f b -> b) -> Stream f m r -> b
streamFold done effect construct (Return x) = done x
streamFold done effect construct (Effect x)
  = effect (streamFold done effect construct <$> x)
streamFold done effect construct (Step x)
  = construct (streamFold done effect construct <$> x)
streamFold done effect construct (Build g) = g done effect construct

export
destroy : (Functor f, Monad m)
       => Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy stream construct effect done = streamFold done effect construct stream


mutual
  public export
  implementation
  (Functor f, Monad m) => Functor (Stream f m) where
    map f x = Build (\r,eff,step => streamFold (r . f) eff step x)

  public export
  implementation
  (Functor f, Monad m) => Applicative (Stream f m) where
    pure = Return
    x <*> y = do f <- x
                 v <- y
                 pure (f v)
  public export
  implementation
  (Functor f, Monad m) => Monad (Stream f m) where
    x >>= k = Build (\r,eff,step => streamFold (streamFold r eff step . k) eff step x)

-- Alternative (Stream f m) where

public export
(Functor f, HasIO m) => HasIO (Stream f m) where
  liftIO act = Effect (liftIO $ io_bind act (pure . Return))

public export
Functor f => MonadTrans (Stream f) where
  lift = Effect . map Return

public export
(Functor f, MonadManaged m) => MonadManaged (Stream f m) where
  use res = lift (use res)

public export -- public, we're exporting Stream currently anyway
%inline
wrap : (Functor f, Monad m) => f (Stream f m a) -> Stream f m a
-- wrap x = Build (\r,eff,step => step $ map (streamFold r eff step) x)
wrap = Step

public export -- public, we're exporting Stream currently anyway
%inline
effect : (Functor f, Monad m) => m (Stream f m r) -> Stream f m r
effect = Effect

-- Does this inspect make sense?
export
inspect : (Functor f, Monad m)
       => Stream f m r -> m (Either r (f (Stream f m r)))
-- inspect (Return x) = pure (Left x)
-- inspect (Effect x) = x >>= inspect
-- inspect (Step x) = pure (Right x)
-- inspect (Build g) = inspect (streamBuild g)
-- If I understand correctly {f} {m} here are still erased, since we're just
-- passing it along in types and we've not bound them in the type sig.
inspect {f} {m} str
  = streamFold
      (pure . Left)
      join
      -- We compress the guts and then reconstruct around it.
      (pure . (Right . map (Effect {f} {m} . map (either Return Step))))
      str

export
unfold : (Functor f, Monad m)
      => (s -> m (Either r (f s))) -> s -> Stream f m r
unfold step s = Effect $ do
    Right fs <- step s
      | Left r => pure (Return r)
    pure (Step (unfold step <$> fs))

-- unfold inspect = id
baf : (Functor f, Monad m) => Stream f m r -> Stream f m r
baf = unfold inspect

-------------------------------------------------
-- Maps
-- Mind that these don't share the same naming scheme as Haskell's 'streaming'
-------------------------------------------------

export
||| map(f): Target the (f)unctor of `Stream f m r`.
mapf : (Functor f, Monad m)
    => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
mapf f s = Build (\r,eff,step => streamFold r eff (step . f) s)

export
||| map(s): Target (s)tream _values_ of `Stream (Of s) m r`.
||| The name is partly to remove clashing with Functor `map`.
maps : Monad m
    => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
maps f s = mapf (first f) s

export
||| map(f)M: Effectfully target the (f)unctor of `Stream f M r`.
mapfM : (Monad m, Functor f)
     => (forall x. f x -> m (g x)) -> Stream f m r -> Stream g m r
mapfM f s = Build (\r,eff,step => streamFold r eff (eff . map step . f) s)

export
||| map(s)M: Effectfully target (s)tream _values_ of `Stream (Of s) M r`.
mapsM : Monad m
    => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapsM f s = mapfM (\(c :> g) => (:> g) <$> f c) s

export
-- | @for@ replaces each element of a stream with an associated stream. Note that the
-- associated stream may layer any functor.
for : (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for str act = Build (\r,eff,step => streamFold r eff
  (\(a :> rest) => streamFold r eff step (act a *> for str act)) str)

  -- where
    -- what : Of a b -> b
    -- what (a :> rest) = streamFold ?fdds ?sdff ?fds (act a *> ?sdf) -- let d = ignore $ act a in streamFold ?sdf ?sddf ?sdff_1 d
  -- loop str = case str of
    -- Return r         -> Return r
    -- Effect m          -> Effect $ liftM loop m
    -- Step (a :> rest) -> do
      -- act a
      -- loop rest

export
-- This is unlikely to be right
decompose : (Functor f, Monad m)
         => Stream (Compose m f) m r -> Stream f m r
-- decompose (Return x) = Return x
-- decompose (Effect x) = Effect $ map decompose x
-- decompose (Step (MkCompose x)) = Effect $ do
  -- str <- x
  -- pure $ Step (decompose <$> str)
-- is this valid? I think so, it closesly matches the Step case above
-- decompose (Build g) = Build (\r,eff,step => g r eff (eff . map step . getCompose))
decompose g
  = Build (\r,eff,step => streamFold r eff (eff . map step . getCompose) g)

export
run : Monad m => Stream m m r -> m r
run (Return x) = pure x
run (Effect x) = x >>= run
run (Step x) = x >>= run
run (Build g) = run (streamBuild g) -- probably reasonable, we're running after all

export
mapsM_ : (Monad m, Functor f) => (forall x . f x -> m x) -> Stream f m r -> m r
mapsM_ f = run . mapf f

||| This specializes to e.g.
||| intercalates :: (Monad m, Functor f) => Stream f m ()
||| -> Stream (Stream f m) m r -> Stream f m r
export
intercalates : (Monad m, Monad (t m), MonadTrans t)
            => t m x -> Stream (t m) m r -> t m r
intercalates sep (Return y) = pure y
intercalates sep (Effect y) = lift y >>= intercalates sep
intercalates sep (Step y) = do
    f' <- y
    steps sep f'
  where
    steps : t m x -> Stream (t m) m r -> t m r
    steps sep (Return y) = pure y
    steps sep (Effect y) = lift y >>= steps sep
    steps sep (Step y) = do
      sep
      f' <- y
      steps sep f'
    steps sep (Build g) = intercalates sep (streamBuild g)
intercalates sep (Build g) = intercalates sep (streamBuild g)


