module Data.AVL

import Data.List

-- Boring basic AVL weight balanced BST
-- ordered on keys to be more versatile

Height : Type
Height = Int

export
data Tree : Type -> Type -> Type where
  Leaf : Tree k a
  Node : (h : Int) -> (key : k) -> (v : a) -> (l : Tree k a) -> (r : Tree k a) -> Tree k a

-- a               a    a
--  \             /  ->  \
--   b   ->  b   b        b
--    \     / \  
--     c   a   c 
rotateL : Tree k a -> Tree k a
rotateL (Node hA kA vA lA (Node hB kB vB lB c)) = Node hB kB vB (Node hA kA vA lA lB) c
rotateL t = t

roLtest1 : rotateL (Node 0 1 'a' Leaf (Node 0 2 'b' Leaf (Node 0 3 'c' Leaf Leaf))) = Node 0 2 'b' (Node 0 1 'a' Leaf Leaf) (Node 0 3 'c' Leaf Leaf)
roLtest1 = Refl


rotateR : Tree k a -> Tree k a
rotateR (Node hA kA vA (Node hB kB vB c rB) rA) = Node hB kB vB c (Node hA kA vA rB rA)
rotateR t = t

roRtest1 : rotateR (Node 0 1 'a' (Node 0 2 'b' (Node 0 3 'c' Leaf Leaf) Leaf) Leaf) = Node 0 2 'b' (Node 0 3 'c' Leaf Leaf) (Node 0 1 'a' Leaf Leaf)
roRtest1 = Refl


--     c       c
--    /       /
--   a   ->  b   -> b
--    \     /      / \
--     b   a      a   c
rotateRL : Tree k a -> Tree k a
rotateRL Leaf = Leaf
rotateRL (Node x k v l r) = rotateR (Node x k v (rotateL l) r)

foobles2 : rotateRL (Node 0 3 'c' (Node 0 1 'a' Leaf (Node 0 2 'b' Leaf Leaf)) Leaf) = Node 0 2 'b' (Node 0 1 'a' Leaf Leaf) (Node 0 3 'c' Leaf Leaf)
foobles2 = Refl

rotateLR : Tree k a -> Tree k a
rotateLR Leaf = Leaf
rotateLR (Node x k v l r) = rotateL (Node x k v l (rotateR r))

foobles3 : rotateLR (Node 0 1 'a' Leaf (Node 0 3 'c' (Node 0 2 'b' Leaf Leaf) Leaf)) = Node 0 2 'b' (Node 0 1 'a' Leaf Leaf) (Node 0 3 'c' Leaf Leaf)
foobles3 = Refl

public export
data NonEmpty : Tree k a -> Type where
  IsNonEmpty : NonEmpty (Node h k v l r)

export
height : Tree k a -> Int
height Leaf = 0
height (Node h k v l r) = h

checkBalance : (t : Tree k a) -> Int
checkBalance Leaf = 0
checkBalance (Node x k v l r) = height r - height l

data Balance = LeftHeavy | LeftLean | Neutral | RightLean | RightHeavy

checkBalance' : Tree k a -> Balance
checkBalance' Leaf = Neutral
checkBalance' t = let i = checkBalance t
               in   if i > 1 then RightHeavy
               else if i == 1 then  RightLean
               else if i == -1 then LeftLean
               else if i < -1 then LeftHeavy
               else Neutral

isLeftHeavy : Tree k a -> Bool
isLeftHeavy t with (checkBalance' t)
  isLeftHeavy t | LeftHeavy = True
  isLeftHeavy t | _ = False

isRightHeavy : Tree k a -> Bool
isRightHeavy t with (checkBalance' t)
  isRightHeavy t | RightHeavy = True
  isRightHeavy t | _ = False

balance : Tree k a -> Tree k a
balance Leaf = Leaf
balance n@(Node h k v l r) = case checkBalance' n of
    LeftHeavy => if isRightHeavy l then rotateRL n else rotateR n
    LeftLean => n
    Neutral => n
    RightLean => n
    RightHeavy => if isLeftHeavy r then rotateLR n else rotateL n

export
insert : Ord k => k -> a -> Tree k a -> Tree k a
insert i x Leaf = Node 1 i x Leaf Leaf
insert i x n@(Node h k v l r) = case compare i k of
    EQ => n
    LT => balance $ Node h k v (insert i x l) r
    GT => balance $ Node h k v l (insert i x r)

export
lookup : Ord k => k -> Tree k a -> Maybe a
lookup i Leaf = Nothing
lookup i n@(Node h k v l r) = case compare i k of
    EQ => Just v
    LT => lookup i l
    GT => lookup i r

export
fromList : Ord k => List (k,a) -> Tree k a
fromList = foldr (uncurry insert) Leaf

bt : Tree Int Char
bt = Node 3 1 '1' Leaf (Node 2 2 '2' (Node 1 3 '3' Leaf Leaf) Leaf)

bt2 : Tree Int Char
bt2 = Node 3 1 '1' (Node 2 2 '2' (Node 1 3 '3' Leaf Leaf) Leaf) Leaf

bt3 : Tree Int Char
bt3 = fromList [(1,'1'),(2,'2'),(3,'3')]

foobles4 : balance (Node 3 1 'a' Leaf (Node 2 3 'c' (Node 1 2 'b' Leaf Leaf) Leaf)) = Node 2 2 'b' (Node 1 1 'a' Leaf Leaf) (Node 1 3 'c' Leaf Leaf)
foobles4 = ?dsfdfs




