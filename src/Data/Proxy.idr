module Data.Proxy

import Data.Stream

data Proxy a = MkProxy

%inline
public export
Functor Proxy where
  map f MkProxy = MkProxy

%inline
public export
Applicative Proxy where
  pure _  = MkProxy
  _ <*> _ = MkProxy

%inline
public export
Alternative Proxy where
  empty = MkProxy
  _ <|> _ = MkProxy

%inline
public export
Monad Proxy where
  _ >>= _ = MkProxy
  join _ = MkProxy

%inline
public export
Semigroup (Proxy a) where
  _ <+> _ = MkProxy

%inline
public export
Monoid (Proxy a) where
  neutral = MkProxy

%inline
public export
Eq (Proxy a) where
  _ == _ = True
  _ /= _ = False

%inline
public export
Ord (Proxy a) where
  compare _ _ = EQ

%inline
public export
Show (Proxy a) where
  show _ = "MkProxy"

public export
Range (Proxy a) where
  rangeFromTo _ _ = [MkProxy]
  rangeFromThenTo _ _ _ = [MkProxy]

  rangeFrom _ = repeat MkProxy
  rangeFromThen _ _ = repeat MkProxy

