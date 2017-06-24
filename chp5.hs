-- | Recursion with pattern matching

maximum1' [] = error "maximum of empty list"
maximum1' [x] = x
maximum1' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum1' xs

-- | use max function

-- | type declaration
-- | type signature
maximum2' :: (Ord a) => [a] -> a

maximum2' [] = error "maximum of empty list"
maximum2' [x] = x
maximum2' (x:xs) = max x (maximum2' xs)

-- | use with guard expressions, if predicate is true then we evaluate to right side

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- | Also notice that we use a guard, but without an otherwise part. That means that if n turns out to be more than 0, the matching will fall through to the next pattern.

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- | Because Haskell supports infinite lists, our recursion doesn't really have to have an edge condition.
-- | The good thing about infinite lists though is that we can cut them where we want. repeat takes an element and returns an infinite list that just has that element.

repeat' :: a -> [a]
repeat' x = x:repeat' x

-- | repeat 3 will never finish evaluating, whereas take 5 (repeat 3)
res = take' 5 (repeat' 3)


-- | https://wiki.haskell.org/Lazy_evaluation
-- | https://hackhands.com/guide-lazy-evaluation-haskell/
{--
Technically, lazy evaluation means call-by-name plus Sharing. A kind of opposite is eager evaluation.
--}


-- | quick sort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
