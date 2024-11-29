-- (**) Replicate the elements of a list a given number of times.Solutions
-- Example:

-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:

-- | test
-- >>> repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> func x n) xs
    where func x 0 = []
          func x n = x: func x (n-1)

repli1 :: [a] -> Int -> [a]
repli1 xs n = foldl (\acc x -> acc ++ func x n ) [] xs 
    where func x 0 = []
          func x n = x: func x (n-1)

-- モナドで考える。
repli2 :: [a] -> Int -> [a]
repli2 xs n = xs >>= replicate n  

-- リストモナドのバインド定義。mapでできることは大体できそう。
-- instance Monad [] where
--     xs >>= f = concat (map f xs)
--     return x = [x]
-- Haskellのリストモナド ([]) のバインド演算子 >>=（bind）は、リストの各要素に関数を適用し、
-- その結果を結合します。これにより、リスト内包表記のような直感的な操作が可能になります。