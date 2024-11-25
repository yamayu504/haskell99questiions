
-- (*) Find the K'th element of a list.Solutions
 

-- The first element in the list is number 1. Example:

-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:

-- >>> elementAt [1,2,3] 2
-- >>> 2
-- >>> elementAt "haskell" 5
-- >>> 'e'
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

elementAt :: [a] -> Int -> a
elementAt _ num 
    | num <= 0 = error "num is 0"
elementAt (x:xs) num = 
    if num == 1
        then x
        else elementAt xs (num-1)

elementAt1 :: [a] -> Int -> a
elementAt1 xs num = xs !! (num -1)


-- 解答メモ

elementAt2 :: [a] -> Int -> a
elementAt2 xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = last $ take n xs -- n個取り出してリストにしてその最後を取り出す

elementAt3 :: [a] -> Int -> a
elementAt3 xs n 
  | length xs < n = error "Index out of bounds"
  | otherwise = head $ drop (n - 1) xs -- 必要のない分だけ前から削除