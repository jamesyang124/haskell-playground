module Chp2 where

-- | Function application (calling a function by putting a space after it and then typing out the parameters) has the highest precedence of them all.
-- | Wrap the function call with parentheses if need

wrapMeNot = div 100 5 * 10
wrapMeLater = div 100 (5 * 10)

doubleMe x = x + x

doubleList z = z ++ z

appendList = 5 : [1,2,3]

-- | [1,2,3] is actually just syntactic sugar for 1:2:3:[]. [] is an empty list.

-- | If you want to get an element out of a list by index, use !!. The indices start at 0.
stringListAtIndexSix = "Steve Buscemi" !! 6

-- | When using head, tail, last and init, be careful not to use them on empty lists.

-- | If a function takes two parameters, we can also call it as an infix function(binary operator) by surrounding it with backticks.

divMeInfix = 92 `div` 10
divMePrefix = div 92 10

{--

Lexically, infix operators consist entirely of "symbols," as opposed to normal identifiers which are alphanumeric (§2.4). Haskell has no prefix operators, with the exception of minus (-), which is both infix and prefix.]

https://www.haskell.org/tutorial/functions.html 3.2

when use infix symbol operators as prefix, surrond parenthesis first:

[] ++ [1,2,3]

(++) [] [1,2,3]

z = (++ [])
z [1,2,3]

[1,2,3]
--}

-- | range(lower..upper) increment by 1

range0 = [2..10]
range1 = [2,4..20]

-- | or range(start,lower..upper) to decrement

range2 = [20,17..1]

-- | Watch out when using floating point numbers in ranges! Because they are not completely precise (by definition), their use in ranges can yield some pretty funky results.

range3 = [3.0, 3.1..3.8]

-- | You can also use ranges to make infinite lists by just not specifying an upper limit.
-- | Because Haskell is lazy, it won't try to evaluate the infinite list immediately because it would never finish.

take24 = take 24 [1,5..]
cycle1 = take 10 (cycle [1,2,3])

-- | list comprehension
listComp = [x*2 | x <- [1..10]]

-- | with predicate

listComp2 = [x*2 | x <- [1..10], x `mod` 3 == 2]

-- | concate filters
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x, x /= 5, x `mod` 7 /= 0]

-- | with multiple ranged vars
boomBangs2 xs ys = [ [x, y] | x <- xs, y <- ys, odd x, even y, x /= 5, y `mod` 7 /= 0]

-- | don't care var => _
-- | We usually use ' to either denote a strict version of a function (one that isn't lazy) or a slightly modified version of a function or a variable.
length' xs = sum [1 | _ <- xs]

-- | because strings are lists, we can use list comprehensions to process and produce strings.
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

{--
A list of numbers is a list of numbers. That's its type and it doesn't matter if it has only one number in it or an infinite amount of numbers. Tuples, however, are used when you know exactly how many values you want to combine and its type depends on how many components it has and the types of the components. They are denoted with parentheses and their components are separated by commas.

Unlike a list(homogeneous), a tuple can contain a combination of several types.
--}

firstOfPair = fst (8,11)

zip2 = zip [1,2,3,4,5] [5,5,5,5,5]
zipLazy = zip [1..] ["apple", "orange", "cherry", "mango"]
