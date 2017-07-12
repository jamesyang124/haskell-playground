module Chp3 where

-- | Haskell has type inference. If we write a number, we don't have to tell Haskell it's a number. It can infer that on its own


-- | :t 'a' => 'a' :: Char
-- | :: is read as "has type of"

-- | :t "Heelo"
-- | "Heelo" :: [Char]


-- | Functions also have types.
-- | From here on, we'll give all the functions that we make type declarations.

noUpperCase x = x
-- | :t noUpperCase
-- | noUpperCase :: t -> t

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- | :t removeNonUppercase
-- | removeNonUppercase :: [Char] -> [Char]
-- | [Char] is type inference by ['A'..'Z']

addThree x y z = x + y + z
-- | type inference by `+` operator:
-- | addThree :: Int -> Int -> Int -> Int

-- | If you want to give your function a type declaration but are unsure as to what it should be, you can always just write the function without it and then check it with :t.
-- | Note that the empty tuple () is also a type which can only have a single value: ()

-- | Functions that have type variables are called polymorphic functions

{--
:t fst
fst :: (a, b) -> a

Note that just because a and b are different type variables, they don't have to be different types. It just states that the first component's type and the return value's type are the same.
--}


-- | A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes.


-- | :t (==)
-- | (==) :: (Eq a) => a -> a -> Bool
{--
We see a new thing here, the => symbol. Everything before the => symbol is called a class constraint.

the equality function takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class (this was the class constraint).
--}

-- | :t elem
-- | elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- | type declaration
-- | type signature
-- | zipHandMade :: (Char a, Char b, Bool c) => a -> b -> c

{--
Notice that in the previous uses of read we did something with the result afterwards. That way, GHCI could infer what kind of result we wanted out of our read

read "5" - 2

read :: (Read a) => String -> a

It returns a type that's part of Read but if we don't try to use it in some way later, it has no way of knowing which type. That's why we can use explicit type annotations. Type annotations are a way of explicitly saying what the type of an expression should be.
--}

res = (read "5" :: Int) + 7
