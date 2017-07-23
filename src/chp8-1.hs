module Shapes
( Point(..)
, Shape3(..)
, surface3
) where

-- | import Data.Derive
-- | import Text.Show.Functions

-- | data means that we're defining a new data type. The part before the = denotes the type, which is Bool. The parts after the = are value constructors.

data Bool = False | True

-- | So we can read this as: the Bool type can have a value of True or False.

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

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

 data Shape2 = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

--}

{--

Let's just say that if we add deriving (Show) at the end of a data declaration, Haskell automagically makes that type part of the Show typeclass

Value constructors are functions, so we can map them and partially apply them and everything.

--}

data Point = Point Float Float deriving (Show)
data Shape3 = Circle3 Point Float | Rectangle3 Point Point deriving (Show)

{--

Notice that when defining a point, we used the same name for the data type and the value constructor.

This has no special meaning, although it's common to use the same name as the type if there's only one value constructor.

--}

surface3 :: Shape3 -> Float
surface3 (Circle3 _ r) = pi * r ^ 2
surface3 (Rectangle3 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

baseCircle :: Float -> Shape3
baseCircle r = Circle3 (Point 0 0) r

baseRect :: Float -> Float -> Shape3
baseRect width height = Rectangle3 (Point 0 0) (Point width height)


{--

You can, of course, export your data types in your modules. To do that, just write your type along with the functions you are exporting and then add some parentheses and in them specify the value constructors that you want to export for it, separated by commas. If you want to export all the value constructors for a given type, just write "..."

see module definition in the beginning of file

By doing Shape(..), we exported all the value constructors for Shape, so that means that whoever imports our module can make shapes by using the Rectangle and Circle value constructors. It's the same as writing Shape (Rectangle, Circle).

We could also opt not to export any value constructors for Shape by just writing Shape in the export statement. That way, someone importing our module could only make shapes by using the auxilliary functions baseCircle and baseRect.

--}

{--

A function that gets some person's first name, a function that gets some person's last name, etc. Well, we'd have to define them kind of like this.

--}

data Person = Person String String Int Float String String deriving (Show)

firstName1 :: Person -> String
firstName1 (Person firstname _ _ _ _ _) = firstname

{--

They included an alternative way to write data types. Here's how we could achieve the above functionality with record syntax.

--}

data Person2 = Person2 { firstName :: String
                       , lastName :: String
                       , age :: Int
                       , height :: Float
                       , phoneNumber :: String
                       , flavor :: String
                       } deriving (Show)


{--

By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.


When we derive Show for the type, it displays it differently if we use record syntax to define and instantiate the type.

--}

data Car = Car String String Int deriving (Show)

{--

ghci> Car "Ford" "Mustang" 1967
Car "Ford" "Mustang" 1967

--}

data Car5 = Car5 {company :: String, model :: String, year :: Int} deriving (Show)

{--

ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}

--}

{--

A value constructor can take some values parameters and then produce a new value. For instance, the Car constructor takes three values and produces a car value. In a similar manner, "type constructors" can take types as parameters to "produce new types".

--}

data Maybe a = Nothing | Just a

{--

Values can have an [Int] type, a [Char] type, a [[String]] type, but you can't have a value that just has a type of [].

--}


{--

Notice that the type of "Nothing" is "Maybe a". Its type is polymorphic. If some function requires a "Maybe Int" as a parameter, we can give it a "Nothing", because a "Nothing" doesn't contain a value anyway and so it doesn't matter.

Similarly, the type of the empty list is "[a]". An empty list can act like a list of anything. That's why we can do [1,2,3] ++ [] and ["ha","ha","ha"] ++ [].

--}

{--

Type parameters are useful because we can make different types with them depending on what kind of types we want contained in our data type.

When we do :t Just "Haha", the type inference engine figures it out to be of the type Maybe [Char], because if the a in the Just a is a string, then the a in Maybe a must also be a string.

--}

{--

Using type parameters is very beneficial, but only when using them makes sense.

If we rewrite:

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

to:

since company, model, year are the get/setter function, so we cannot reuse on diff value constructor in same module

--}

data Car6 a b c = Car6 { company6 :: a
                     , model6:: b
                     , year6 :: c
                     } deriving (Show)

{--

THen we have to write its all type signature instead from:

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


to:

--}

tellCar :: (Show a) => Car6 String String a -> String
tellCar (Car6 {company6 = c, model6 = m, year6 = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{--

We'd have to force this function to take a Car type of (Show a) => Car String String a.

If we were defining a mapping type, we could add a typeclass constraint in the data declaration:

data (Ord k) => Map k v = ...

However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations. Why? Well, because we don't benefit a lot, but we end up writing more class constraints, even when we don't need them.

If we don't put the constraint in the data declaration, we don't have to put (Ord k) => in the type declarations of functions that don't care whether the keys can be ordered or not.

--}

{--

An example of such a function is "toList", that just takes a mapping and converts it to an associative list.

Its type signature is "toList :: Map k a -> [(k, a)]". If "Map k v" had a type constraint in its data declaration, the type for toList would have to be
"toList :: (Ord k) => Map k a -> [(k, a)]"", even though the function doesn't do any comparing of keys by order.

--}

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

{--

Once again, it's very important to distinguish between the type constructor and the value constructor.

When declaring a data type, the part before the = is the type constructor and the constructors after it (possibly separated by |'s) are value constructors.

Giving a function a type of Vector t t t -> Vector t t t -> t would be wrong, because we have to put types in type declaration and the vector type constructor takes only one parameter, whereas the value constructor takes three. Let's play around with our vectors.

--}
