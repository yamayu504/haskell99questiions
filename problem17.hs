-- (*) Split a list into two parts; the length of the first part is given.Solutions
-- Do not use any predefined predicates.
-- Example:
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
-- Example in Haskell:

-- | test
-- >>> split "abcdefghik" 3
-- ("abc" , "defghik")
-- >>> split1 "abcdefghik" 3
-- ("abc" , "defghik")

split :: [a] -> Int -> ([a],[a])
split xs n
    | length xs < n = (xs,[])
    | otherwise = (take n xs,drop n xs)

split1 :: [a] -> Int -> ([a],[a])
split1 lst n = snd $ foldl helper (1, ([],[])) lst
    where helper (i,acc) x = if n <= i
                             then (i+1,(fst acc,snd acc ++[x]))
                             else (i+1,(fst acc ++[x],snd acc))