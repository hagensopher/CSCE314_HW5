


data Tree a b = Branch b (Tree a b) (Tree a b)
                | Leaf a
-- ask the prof what is this and line 10 mysterious syntax
showTree :: (Show a, Show b) => Tree a b -> String
showTree (Leaf a) = ['\t'] ++ show a -- i want this to go backwards one if it is the last one???
showTree (Branch b l r) = show b ++ "\n\t" ++ showTree l ++ "\n\t" ++showTree r 

--why do new lines stop working what is wrong with this
showTree2 :: (Show a, Show b) => Tree a b  -> Int -> String
showTree2 (Leaf a)  f = ['\n'] ++ extend  ++ show a
    where extend = foldr (++) " " ([" " | x<-[1..f]])
showTree2 (Branch b l r)  f = ['\n'] ++ show b ++ extend ++ showTree2 l (f+8) ++ extend ++showTree2 r (f+8)
    where extend = foldr (++) "\n" ([" " | x<-[1..f]])


instance (Show a, Show b) => Show (Tree a b) where
    show = showTree


mytree = Branch "A" (Branch "B" (Leaf (1::Int)) (Leaf (2::Int))) (Leaf (3::Int))

leafCollection :: Tree a b  -> [a] --needs to be a common type c
leafCollection (Leaf a) = [a]
leafCollection (Branch b l r) = leafCollection l ++ leafCollection r

bcol :: Tree a b  -> [b] --needs to be a common type c
bcol (Leaf a) = []
bcol (Branch b l r) = [b] ++ bcol l ++ bcol r
mapleaves leaves = map show leaves --makes a common type c?
mapbranches branch = map show branch --makes a common type c?

--preorder takes a function 1 which gets all the leaves to common type c
-- function two gets all the branches to common type c 
--preorder :: (a->c) -> (b->c) -> Tree a b -> [c]
