-- (**) Extract a slice from a list.Solutions
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1.
-- Example:
-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
-- Example in Haskell:

-- | test
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs n m 
    | length xs < m = error "error"
    | otherwise = drop n (take m xs)

slice1 :: [a] -> Int -> Int -> [a]
slice1 xs n m = reverse . snd $ foldl helper (0,[]) xs
        where 
            helper (i,acc) x 
                | i >= n && i<= m = (i+1,x:acc)
                | otherwise =  (i+1,acc)