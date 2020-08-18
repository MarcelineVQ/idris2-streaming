module Util

import System.File

infixl 1 <&>
export
(<&>) : Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x

infixl 4 <$,$>

export
(<$) : Functor f => a -> f b -> f a
x <$ y = map (const x) y

export
($>) : Functor f => f a -> b -> f b
($>) = flip (<$)

infixr 1 =<<
export
(=<<) : Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- For when Lazy is causing type problems
infixr 4 &&|
export
(&&|) : Bool -> Bool -> Bool
(&&|) x y = x && y

export
catMaybes : List (Maybe a) -> List a
catMaybes z = foldr (\m,f => maybe f (\x => (x ::) . f) m) id z []

export
isJust : Maybe a -> Bool
isJust (Just _ ) = True
isJust _ = False

export
unzip : List (a,b) -> (List a, List b)
unzip = foldr (\(x,y),(xs,ys) => (x :: xs, y :: ys)) ([],[])

-- This is hiiiiiideously slow! maybe it's because I'm using it at elab-time
export
unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 [] = ([],[],[])
unzip3 ((x, (y, z)) :: ls) = let (xs,ys,zs) = unzip3 ls
                             in (x :: xs, y:: ys, z :: zs)

export
unless : Applicative f => Bool -> Lazy (f ()) -> f ()
unless b act = if b then pure () else act

public export
monus : Nat -> Nat -> Nat
monus Z _ = Z
monus k Z = k
monus (S k) (S j) = k `monus`j

export
withFile : HasIO io => String -> Mode -> (File -> io a) -> io (Either FileError a)
withFile file mode act = do Right f <- openFile file mode
                              | Left err => pure (Left err)
                            r <- act f
                            closeFile f
                            pure (Right r)
