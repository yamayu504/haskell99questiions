-- (*) Find out whether a list is a palindrome.Solutions
-- Hint: A palindrome can be read forward or backward; e.g. (x a m a x).
-- Example in Haskell:

-- | test
-- >>> isPalindrome3 [1,2,3]
-- False
-- >>> isPalindrome3 "madamimadam"
-- True
-- >>> isPalindromeM [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a)=> [a] -> Bool
isPalindrome xs = all (\(a,b)-> a == b ) (zip xs (reverse xs))

isPalindrome1 :: (Eq a)=>[a] -> Bool
isPalindrome1 xs =  xs == reverse xs

isPalindrome2 :: (Eq a)=>[a] -> Bool
isPalindrome2 xs = and $ zipWith (==) xs (reverse xs)

isPalindrome3 :: (Eq a)=>[a] -> Bool
isPalindrome3 []  = True
isPalindrome3 [x] = True
isPalindrome3 (x:xs)  = (x == last xs) && isPalindrome3 (init xs)

-- 解答

isPalindromeM :: (Eq a) => [a] -> Bool
isPalindromeM   = reverse  >>= (==)  
--　関数モナド((->)r)を利用している。関数モナドは、下の型を持つ。
-- つまり、左の関数を適用した値と元の値を右の関数に適応する。
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
-- f >>= g = \x -> g (f x) x
