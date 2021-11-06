

data Tree a b = Branch b (Tree a b) (Tree a b)
                | Leaf a

showTree :: (Show a, Show b) => Tree a b  -> String
showTree (Leaf a) = ['\t'] ++ show a
showTree (Branch b l r) = show b ++ "\n\t" ++ showTree l ++ "\n\t" ++showTree r

instance (Show a, Show b) => Show (Tree a b) where
    show = showTree
