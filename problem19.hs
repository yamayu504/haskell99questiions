import System.FilePath (combine)
-- (**) Rotate a list N places to the left.Solutions
-- Hint: Use the predefined functions length and (++).
-- Examples:
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:

-- | test
-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
-- >>> rotate2 ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs n = 
    let tupple =  splitAt (rotateIndex xs n) xs
    in snd tupple ++ fst tupple

rotateIndex :: [a] -> Int -> Int
rotateIndex xs n
    | n < 0 = length xs + n
    | otherwise = n

rotate1 :: [a] -> Int -> [a]
rotate1 xs 0 = xs
rotate1 (x:xs) n = rotate1 (xs++[x]) (n-1) 

-- 解答
rotate2 :: [a] -> Int -> [a]
rotate2 [] _ = []
rotate2 xs 0 = xs
rotate2 (x:xs) n = rotate2 (xs ++ [x]) (n-1)
rotate2 xs n = rotate2 xs (length xs + n)

-- 無限ループに入ると次の行のパターンマッチングを行う。
-- 今回は、4行目がn<0の場合無限ループに入る危険性があり、5行目に評価が移る
-- 非推奨なので注意