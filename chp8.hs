-- | data means that we're defining a new data type. The part before the = denotes the type, which is Bool. The parts after the = are value constructors.

data Bool = False | True

-- | So we can read this as: the Bool type can have a value of True or False.

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- | The Rectangle value constructor has four fields which accept floats.

-- | Now when I say fields, I actually mean parameters. Value constructors are actually functions that ultimately return a value of a data type. Let's take a look at the type signatures for these two value constructors.

{--

:t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape

--}

-- | The first notable thing here is the type declaration. It says that the function takes a shape and returns a float. We couldn't write a type declaration of Circle -> Float because Circle is not a type, Shape is.

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


res1 = surface $ Circle 10 20 10

{--

Remember, when we try to print a value out in the prompt, Haskell first runs the show function to get the string representation of our value and then it prints that out to the terminal. To make our Shape type part of the Show typeclass, we modify it like this:

 --}

 data Shape2 = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)


{--

Let's just say that if we add deriving (Show) at the end of a data declaration, Haskell automagically makes that type part of the Show typeclass

--}
