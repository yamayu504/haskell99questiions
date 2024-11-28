-- (*) Duplicate the elements of a list.Solutions
-- Example:
-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
-- Example in Haskell:

-- | test
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
-- >>> dupli1 [1, 2, 3]
-- [1,1,2,2,3,3]
-- >>> dupli2 [1, 2, 3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli1 :: [a] -> [a]
dupli1  = concatMap (\x -> x : [x])

dupli2 :: [a] -> [a]
dupli2 = foldr (\x acc -> x:x:acc ) []

-- 解答
-- m a -> (a -> m b) -> m b
-- リストモナドは、concatMapになるので、リストを渡すと一つのリストに統合される。
dupli3 :: [a] -> [a]
dupli3 xs = xs >>= (\x -> [x,x])