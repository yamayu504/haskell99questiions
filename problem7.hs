-- (**) Flatten a nested list structure.Solutions
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- Example:
-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
-- Example in Haskell:

-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

-- | test
-- >>>flatten (Elem 5)
-- [5]
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- >>> flatten (List [])
-- []

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List [])     = [] 
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten1 :: NestedList a -> [a]
flatten1 (Elem x)  = [x]
flatten1 (List xs) = concatMap flatten xs
-- concateMapもうちょっとすぐ出てくるようにしたい。

-- 回答見た

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List xs) = foldr (++) [] $ map flatten2 xs 
-- 右側から値を入れていく。結局mapで回帰させる。

flatten4 = reverse . rec []
  where
  rec acc (List []) = acc
  rec acc (Elem x)  = x:acc
  rec acc (List (x:xs)) = rec (rec acc x) (List xs)
-- accを定義して、accに値を入れていく。