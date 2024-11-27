import Data.List
-- (*) Run-length encoding of a list.Solutions
-- Use the result of Problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- Example:
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:

-- | test
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]


encode :: String -> [(Int,Char)]
encode xs = map (\x -> (length x, head x)) $ group xs

-- 解答
encode1 :: String -> [(Int,Char)]
encode1 xs = [(length x, head x) | x <- group xs]
-- リスト糖衣構文