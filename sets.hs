type Set a = [a]

mkSet:: (Eq a, Ord a) => [a] -> Set a 
mkSet [] = []
mkSet (x:xs) = if x `elem` xs then mkSet xs else x : mkSet xs

subset:: Eq a => Set a -> Set a -> Bool 
subset set1 set2 = set1 == [x | x <- set2, x `elem` set1]

setEqual:: Eq a => Set a -> Set a -> Bool 
setEqual set1 set2 = if subset set1 set2 then length set1 == length set2 else False
--not ordered right :(
setProd2:: (Eq t, Eq t1) => Set t -> Set t1 -> Set (t,t1)
setProd2 set1 set2 = zip (take l (cycle set1)) (take l (cycle set2))
                    where l = lcm (length set1) (length set2)

--trying it again where sets is ordered 
--myfunction from homework 3 drills
setProd:: (Eq t, Eq t1) => Set t -> Set t1 -> Set (t,t1)
setProd set1 set2 = zip (concatMap (replicate (length set2)) set1) (take l (cycle set2))
                        where l = lcm (length set1) (length set2)

partitionSet :: Eq t => Set t -> Set( Set (Set t))
partitionSet set1 = 