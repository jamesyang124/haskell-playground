module Chp4 where

-- | pattern matching for functions
-- | very similar as you define function with values as input, instead of type variables

-- | the output type should be consistent. Type inference will check for it

pm 3 4 = "7"
pm 1 2 = "3"
pm x y = "Other then else"

-- | for triplet tuple, we can wild card first 2 values, and return only 3rd.
third (_, _, z) = z

-- | you can also pattern match in list comprehensions.
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]

v = [a+b | (a,b) <- xs]
-- | [4,7,6,8,11,4]

-- | A pattern like x:xs will bind the head of the list to x and the rest of it to xs, even if there's only one element so xs ends up being an empty list.
dehead (y:_) = y

-- | length' :: (Num b) => [a] -> b
-- | [] is empty list, [1,2,3] === 1 : 2 : 3 : []
length' [] = 0
length' (_:xs) = 1 + length' xs

-- | as patterns => @, so we can use variable z@y to as whole y structure

capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

{--
Guards are indicated by pipes that follow a function's name and its parameters. Usually, they're indented a bit to the right and lined up. A guard is basically a boolean expression. If it evaluates to True, then the corresponding function body is used. If it evaluates to False, checking drops through to the next guard and so on.
--}

-- | instead of matching a concrete value, we use a condional check to match a response

bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"

-- | Many times, the last guard is otherwise. otherwise is defined simply as otherwise = True and catches everything.

max' a b | a > b = a | otherwise = b

{--
We put the keyword where after the guards (usually it's best to indent it as much as the pipes are indented) and then we define several names or functions. These names are visible across the guards and give us the advantage of not having to repeat ourselves.
--}

-- | bmiTell :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- | You can also use where bindings to pattern match
-- | where bindings aren't shared across function bodies of different patterns. If you want several patterns of one function to access some shared name, you have to define it globally.

initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- | Just like we've defined constants in where blocks, you can also define functions(bmi weight height = weight / height ^ 2).

calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

res = calcBmis [(180, 50)]

-- | let bindings, you can bind local variables/functions and use it in next in expression
-- | form: let <bindings> in <expression>

letBindings1 = 4 * (let a = 9 in a + 1) + 2

-- | we can also introduce function in bindings

letBindings2 = [let square x = x * x in (square 5, square 3, square 2)]

-- | We include a let inside a list comprehension much like we would a predicate, only it doesn't filter the list, it only binds to names.

calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

{--
The names defined in a let inside a list comprehension are visible to the output function (the part before the |) and all predicates and sections that come after of the binding.

We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.
--}

-- | since let bindings are expressions and are fairly local in their scope, they can't be used across guards. Some people prefer where bindings because the names come after the function they're being used in. That way, the function body is closer to its name and type declaration and to some that's more readable.


-- | case expressions are, well, expressions, much like if else expressions and let bindings.

head' [] = error "No head for empty lists!"
head' (x:_) = x

-- | above equivalent

head1' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

-- | Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere.

describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

{--
They are useful for pattern matching against something in the middle of an expression. Because pattern matching in function definitions is syntactic sugar for case expressions, we could have also defined this like so:
--}
describeList2 xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
