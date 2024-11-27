-- (**) Pack consecutive duplicates of list elements into sublists.Solutions
-- If a list contains repeated elements they should be placed in separate sublists.
-- Example:
-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:

import Data.List

-- | test
-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
-- >>> pack1 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
-- >>> pack2 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: String ->[String]
pack  = group

pack1 :: String ->[String]
pack1  = foldr func []
  where func x [] = [[x]]
        func x (y:xs) =
          if head y == x
            then (x : y):xs
            else [x] : (y:xs)

-- 解答

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : takeWhile (==x) xs) : pack2 (dropWhile (==x) xs)
-- takeWhileは、先頭から条件に合うまで並べる。なので、条件が合わなくなったところで検索が終わる。
-- dropWhileについても同じ。