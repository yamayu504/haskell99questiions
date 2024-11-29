-- (**) Drop every N'th element from a list.Solutions
-- Example:
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:

-- | test
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
-- >>> dropEvery1 "abcdefghik" 3
-- "abdeghk"


-- 解答
dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
    if length xs < n 
        then xs
        else take (n-1) xs ++ dropEvery (drop n xs) n
        
--これすごくきれい。もうちょっと分解して考えたほぅがいいな。

dropEvery1 :: [a] -> Int -> [a]
dropEvery1 lst n = snd $ foldl helper (1, []) lst
    where helper (i,acc) x = if n == i
                             then (1,acc)
                             else (i+1,acc++[x])
-- これかなり命令形のループの考え方に近い。