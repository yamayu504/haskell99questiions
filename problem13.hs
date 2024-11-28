-- (**) Run-length encoding of a list (direct solution).Solutions
-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
--  As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- Example:

-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

-- | test
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- >>> encodeDirect2 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- >>> encodeDirect1 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Multiple Int a | Single a
  deriving (Show)

encodeDirect :: Eq a => [a] ->[ListItem a]
encodeDirect  = foldr func []
  where 
    func x [] = [Single x]
    func x ((Single a):xs) =
          if x == a
            then Multiple 2 a : xs
            else Single x : Single a:xs
    func x ((Multiple b a):xs) =
          if x == a
            then Multiple (b+1) a:xs
            else Single x : Multiple b a:xs

encodeDirect2 :: Eq a => [a] -> [ListItem a]
encodeDirect2 = foldr func []
  where
    func x [] = [Single x]  -- リストが空の場合、x を `Single` として追加
    func x ((Single a):xs)
      | x == a    = Multiple 2 a : xs  -- 同じ要素が続く場合、`Multiple` を作成
      | otherwise = Single x : Single a : xs  -- 異なる場合、`Single` をそのまま
    func x ((Multiple n a):xs)
      | x == a    = Multiple (n + 1) a : xs  -- `Multiple` の数を増やす
      | otherwise = Single x : Multiple n a : xs  -- 異なる場合、`Single` として追加

-- 解答

encodeDirect1 :: Eq a => [a] -> [ListItem a]
encodeDirect1 []     = []
encodeDirect1 (x:xs) = 
    let (group, rest) = span (==x) xs 
        in 
        convertIfSingle (Multiple (1 + length group) x) : encodeDirect1 rest
    where convertIfSingle (Multiple 1 x) = Single x
          convertIfSingle (Multiple ｂ x) = Multiple ｂ x
-- span
-- span (<3) [1, 2, 3, 4, 1, 2]  -- ([1, 2], [3, 4, 1, 2])こんな感じに使える。
-- 左から条件検索して一致しない時点で右側に分類される。
-- (group, rest) = span (==x) xs これでパターンマッチングしている。
