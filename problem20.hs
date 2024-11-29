-- (*) Remove the K'th element from a list.Solutions
-- Example in Prolog:
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- Example in Lisp:
-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
-- Example in Haskell:

-- | test
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
-- >>> removeAt1 3 "abcd"
-- ('c',"abd")

removeAt :: Int -> [a] ->(a,[a])
removeAt n xs =
    let (first, second) = splitAt n xs
    in (last first,init first ++ second)

removeAt1 :: Int -> [a] ->(a,[a])
removeAt1 1 (x:xs) = (x, xs)
removeAt1 n (x:xs) = (l, x:r)
    where (l, r) = removeAt (n - 1) xs
-- 回帰でnが1のときの(l,r)を取得して、

removeAt2 :: Int -> [a] ->(a,[a])
removeAt2 n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
-- !!の操作を使ってないので使っていきたい。