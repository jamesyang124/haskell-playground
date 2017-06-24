-- | Putting a space between two things is simply function application. The space is sort of like an operator and it has the highest precedence.

{--
max :: (Ord a) => a -> a -> a
can be written as
max :: (Ord a) => a -> (a -> a)
--}

-- | if we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out.

-- | What really happens when we do multThree 3 5 9 or ((multThree 3) 5) 9? First, 3 is applied to multThree, because they're separated by a space. That creates a function that takes one parameter and returns a function.

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- | assume we have type variable x in below function definition
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- | Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side.

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

{--
The only special thing about sections is using -. From the definition of sections, (-4) would result in a function that takes a number and subtracts 4 from it. However, for convenience, (-4) means minus four.

So if you want to make a function that subtracts 4 from the number it gets as a parameter, partially apply the subtract function like so: (subtract 4).
--}

-- | multThree x y z = x * y * z
-- | multThree 3 4 function is not instance of Show type, so cannot print out
-- | that partial applied function, but we can :t multThree 3 4


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- | First of all, notice the type declaration. Before, we didn't need parentheses because -> is naturally right-associative. However, here, they're mandatory. They indicate that the first parameter is a function that takes something and returns that same thing.


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- |  Flip simply takes a function and returns a function that is like our original function, only the first two arguments are flipped.

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- | Here, we take advantage of the fact that functions are curried. When we call flip' f without the parameters y and x, it will return an f that takes those two parameters but calls them flipped. (b -> a -> c)

flip2' :: (a -> b -> c) -> b -> a -> c
flip2' f y x = f x y

-- | map takes a function and a list and applies that function to every element in the list, producing a new list. Let's see what its type signature is and how it's defined.

map3 :: (a -> b) -> [a] -> [b]
map3 _ [] = []
map3 f (x:xs) = f x : map f xs

-- | use guard expression, If p x evaluates to True, the element gets included in the new list.

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs)
    | p x       = x : filter2 p xs
    | otherwise = filter2 p xs

-- | Because we only end up using the head of the filtered list, it doesn't matter if the filtered list is finite or infinite. The evaluation stops when the first adequate solution is found.

-- | since filter may reduce to (x : filter2 p xs) as WHNF when p x is true
-- | thanks to WHNF now topmost evaluation:
-- | head (x: filter2 p xs) has been in WHNF as well
-- | then we can evaludate to NF by head function's pattern matching
{--
head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead
--}

largestDivisible :: (Integral a) => a
largestDivisible = head (filter2 p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- | infiniteList = filter2 p [100000,99999..]
-- |                  where p x = x `mod` 3829 == 0


-- | https://wiki.haskell.org/Weak_head_normal_form
-- | https://en.wikibooks.org/wiki/Haskell/Graph_reduction

sum2 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
sum3 = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

{--

For our next problem, we'll be dealing with Collatz sequences. We take a natural number. If that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that.

 So if we take the starting number 13, we get this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 10 terms

 for all starting numbers between 1 and 100, how many chains have a length greater than 15?
--}

-- | define chain function first
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- | map (*) [0..] produces a list like the one we'd get by writing [(0*),(1*),(2*),(3*),(4*),(5*)...

{--
let listOfFuns = map (*) [0..]
ghci> (listOfFuns !! 4) 5

Getting the element with the index 4 from our list returns a function that's equivalent to (4*)
--}

-- | To make a lambda, we write a \ (because it kind of looks like the greek letter lambda if you squint hard enough) and then we write the parameters, separated by spaces. After that comes a -> and then the function body.

numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- | Lambdas are expressions, that's why we can just pass them like that. The expression (\xs -> length xs > 15) returns a function that tells us whether the length of the list passed to it is greater than 15.

{--

And like normal functions, you can pattern match in lambdas. The only difference is that you can't define several patterns for one parameter, like making a [] and a (x:xs) pattern for the same parameter and then having values fall through. If a pattern matching fails in a lambda, a runtime error occurs, so be careful when pattern matching in lambdas!

--}

addThree3 x y z = x + y + z

-- | return lambda as curried function, but less readable

addThree4 = \x -> \y -> \z -> x + y + z

-- | but sometimes it might be more readable
flip5' :: (a -> b -> c) -> b -> a -> c
flip5' f = \x y -> f y x

sum5' :: (Num a) => [a] -> a
sum5' xs = foldl (\acc x -> acc + x) 0 xs

-- | but we can improve that as

sum6' :: (Num a) => [a] -> a
sum6' = foldl (+) 0

{--

The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.

--}

-- | scan left or right may have difference

s1 = scanl (+) 0 [3,5,2,1]
-- | [0,3,8,10,11]
s2 = scanr (+) 0 [3,5,2,1]
-- | [11,8,3,1,0]

{--
Alright, next up, we'll take a look at the $ function, also called function application. First of all, let's check out how it's defined:

($) :: (a -> b) -> a -> b
f $ x = f x

--}

{--

hereas normal function application (putting a space between two things) has a really high precedence, the $ function has the lowest precedence. Function application with a space is left-associative (so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.

--}

-- | might help you define your fucntion application order

ss1 = sum (map sqrt [1..130])
-- | save the parenthesess
ss2 = sum $ map sqrt [1..130]

-- | When a $ is encountered, the expression on its right is "applied as the parameter" to the function on its left.

se1 = sqrt (3 + 4 + 9)
-- | $ has the lowest precedence for left now, so 3 + 4 + 9 will evaluate first
se2 = sqrt $ 3 + 4 + 9

-- | because $ is right-associative, f (g (z x)) is equal to f $ g $ z x.

{--
    f $ g $ z x
=>  f (g $ z x)
=>  f (g (z x))
--}

-- | so we can rewrite sum (filter (> 10) (map (*2) [2..10]))
-- | to sum $ filter (> 10) $ map (*2) [2..10]

-- | But apart from getting rid of parentheses, $ means that function application can be treated just like another function. That way, we can, for instance, map function application over a list of functions.

res5 = map ($ 3) [(4+), (10*), (^2), sqrt]
-- | [7.0,30.0,9.0,1.7320508075688772]

-- | We do function composition with the . function, which is defined like so:

{--

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

--}


res7 = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
res8 = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- | Function composition is right-associative
-- | f (g (z x)) == (f . g . z) x
{--
    (f. g . z) x
=>  ((\x -> f(g x)) . z) x
=>  (\t -> (\x -> f(g x)) (z t)) x
=>  WHNF, but reduce lambda \t .. by x
=>  (\x -> f(g x)) (z x)
=>  WHNF, but reduce lambda \x .. by (z x)
=>  f (g (z x))

--}

{--

However, many times, writing a function in point free style can be less readable if a function is too complex.

The prefered style is to use let bindings to give labels to intermediary results or split the problem into sub-problems and then put it together so that the function makes sense to someone reading it instead of just making a huge composition chain.

--}

-- | point free style
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- | point as composition
oddSquareSumP = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]


-- | let bindings to split it in sub-problem domain
oddSquareSumL =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
