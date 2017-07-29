module Chp92
where

{-- when

The when function is found in Control.Monad (to get access to it, do import Control.Monad). It's interesting because in a do block it looks like a control flow statement, but it's actually a normal function.

It takes a boolean value and an I/O action if that boolean value is True, it returns the same I/O action that we supplied to it.

However, if it's False, it returns the return (), action, so an I/O action that doesn't do anything.

--}

import Control.Monad
import Data.Char

main1 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main1

{-- review $ again

$ just a way to group the computation, the parenthesis not have any implication of prcedence, but the operators and function application have, and $ also.

$ will wrap right part all together, and apply its left hand function to right hand side value when the precedence of $ meet as highest priority:

map                     :: (a->b) -> [a] -> [b]
map f  []               =  []
map f (x:xs)            =  f x : map f xs

Function application has higher precedence than any infix operator, and thus the right-hand side of the second equation:

f x : map f xs

parses as:

(f x) : (map f xs)


(* 1) $ 2 + 5
=> (\x -> x * 1) $ 2 + 5

-- due to $'s low precedence, (x => x * 1) $ 2 will not apply,
-- so 2 + 5 will take higher precedence

=> (\x -> x * 1) $ 7
=> (\x -> x * 1) 7
=> 7 * 1
=> 7


The whole expression will still be deduct from left right each time and evaludate by each operation precedence.

--}

res1 = ((-) 1) $ 1 * 5
res2 = 2 + 5 * 1
res3 = (* 1) $ 2 + 5
res4 = (+ 1) $ (* 3) $ (^ 2) $ 5 ^ 2 + 3 - 1 * 2

{--

(+ 1) $ (* 3) $ (^ 2) $ 5 ^ 2 + 3 - 1 * 2

since $ is right-associative, and almost lowest precedence

(+ 1) $ (* 3) $ (^ 2) $ 5 ^ 2 + 3 - 1 * 2
=> (\x -> x + 1) $ (* 3) $ (^ 2) $ 5 ^ 2 + 3 - 1 * 2
=> (\x -> x + 1) $ (\y -> y * 3) $ (^ 2) $ 5 ^ 2 + 3 - 1 * 2
=> (\x -> x + 1) $ (\y -> y * 3) $ (\z -> z ^ 2) $ 5 ^ 2 + 3 - 1 * 2
-- $ take lowest pred.
=> ... $ 25 + 3 - 1 * 2
=> ... $ 25 + 3 - 2
=> ... $ 28 - 2
=> ... $ 26
=> ... $ (\z -> z ^ 2) $ 26
=> ... $ 676
=> ... $ (\y -> y * 3) $ 676
=> ... $ 2028
=> (\x -> x + 1) $ 2028
=> 2029

--}

main2 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

main3 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

{-- sequence

sequence :: [IO a] -> IO [a]

takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other.

main2 is the same as main3

So sequence [getLine, getLine, getLine] makes an I/O action that will perform getLine three times.

--}

{--

example:

ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]

What's with the [(),(),(),(),()] at the end?

Well, when we evaluate an I/O action in GHCI, it's performed and then its result is printed out, unless that result is (), in which case it's not printed out.

That's why evaluating putStrLn "hehe" in GHCI just prints out hehe (because the contained result in putStrLn "hehe" is ()).

But when we do getLine in GHCI, the result of that I/O action is printed out, because getLine has a type of IO String.

--}

res5 = mapM print [1,2,3]
-- :t print
-- print :: Show a => a -> IO ()

-- :t mapM
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

-- res5 => [(), (), ()]

res6 = mapM_ print [1,2,3]
-- :t mapM_
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

{--

mapM and mapM_ were introduced.

mapM takes a function and a list, maps the function over the list and then sequences it.

mapM_ does the same, only it throws away the result later. We usually use mapM_ when we don't care what result our sequenced I/O actions have.

--}

{--

forever takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.

forM (located in Control.Monad) is like mapM, only that it has its parameters switched around. The first parameter is the list and the second one is the function to map over that list

--}

main4 = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

main5 = do
  colors <- forM [1,2,3,4] (\a -> do
      putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
      color <- getLine
      return color)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors

{--

The (\a -> do ... ) is a function that takes a number and returns an I/O action. We have to surround it with parentheses, otherwise the lambda thinks the last two I/O actions belong to it. (putStrLn and mapM)

for "return color" We actually didn't have to do that, because getLine already has that contained within it.

Doing color <- getLine and then return color is just unpacking the result from getLine and then repackaging it again, so it's the same as just doing getLine.

--}
