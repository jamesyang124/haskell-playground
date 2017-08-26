{-- The newtype keyword

The value constructor of a newtype must have exactly one field, and only one value constructor

The newtype keyword in Haskell is made exactly for these cases when we want to just take one type and wrap it in something to present it as another type. In the actual libraries,

ZipList a is defined like this:

newtype ZipList a = ZipList { getZipList :: [a] }

If you use the data keyword to wrap a type, there's some overhead to all that wrapping and unwrapping when your program is running.

But if you use newtype, Haskell knows that you're just using it to wrap an existing type into a new type (hence the name), because you want it to be the same internally but have a different type.

With that in mind, Haskell can get rid of the wrapping and unwrapping once it resolves which value is of what type.

We can also use the deriving keyword with newtype just like we would with data. Because newtype just wraps an existing type.

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

In this particular newtype, the value constructor CharList (rihght side of equation) has the following type:

CharList :: [Char] -> CharList

Conversely, the getCharList function, which was generated for us because we used record syntax in our newtype, has this type:

getCharList :: CharList -> [Char]

--}

-- data Person = Person String String Int Float String String deriving (Show)

-- :t Person
-- Person :: String -> String -> Int -> Float -> String -> String -> Person

-- :t firstName
-- firstName :: Person -> String

-- syntax sugar like:
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
-- syntax sugar as:

data Person = Person { firstName :: String
                      , lastName :: String
                      , age :: Int
                      , height :: Float
                      , phoneNumber :: String
                      , flavor :: String
                      } deriving (Show)

{--

With Maybe, we just say instance Functor Maybe where because only type constructors that take exactly one parameter can be made an instance of Functor.

But it seems like there's no way to do something like that with (a,b) so that the type parameter a ends up being the one that changes when we use fmap.

To get around this, we can newtype our tuple in such a way that the second type parameter represents the type of the first component in the tuple:

newtype Pair b a = Pair { getPair :: (a,b) }

:t Pair
Pair :: (a, b) -> Pair b a

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

We pattern match to get the underlying tuple, then we apply the function f to the first component in the tuple and then we use the Pair value constructor to convert the tuple back to our Pair b a.

If we imagine what the type fmap would be if it only worked on our new pairs, it would be:

fmap :: (a -> b) -> Pair c a -> Pair c b

So now, if we convert a tuple into a Pair b a, we can use fmap over it and the function will be mapped over the first component:

ghci> getPair $ fmap (*100) (Pair (2,3))
(200,3)
-- ONLY FIRST ELEM IS FMAPPED

ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)

--}

{--

Haskell can represent the values of types defined with newtype just like the original ones, only it has to keep in mind that the their types are now distinct.

This fact means that not only is newtype faster, it's also lazier. Let's take a look at what this means.

The undefined value in Haskell represents an erronous computation. If we try to evaluate it (that is, force Haskell to actually compute it) by printing it to the terminal, Haskell will throw an exception

However, if we make a list that has some undefined values in it but request only the head of the list, which is not undefined, everything will go smoothly because Haskell doesn't really need to evaluate any other elements in a list if we only want to see what the first element is:

head [3,4,5,undefined,2,undefined]
--}

{--

Let's make a function that pattern matches on a CoolBool and returns the value "hello" regardless of whether the Bool inside the CoolBool was True or False:

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

ghci> helloMe undefined
"*** Exception: Prelude.undefined

Types defined with the data keyword can have "multiple value constructors" (even though CoolBool only has one).

So in order to see if the value given to our function conforms to the (CoolBool _) pattern, Haskell has to evaluate the value just enough to see which value constructor was used when we made the value.

And when we try to evaluate an undefined value, even a little, an exception is thrown.

newtype CoolBool = CoolBool { getCoolBool :: Bool }

ghci> helloMe undefined
"hello"

when we use newtype, Haskell can internally represent the values of the new type in the same way as the original values.

It doesn't have to add another box around them, it just has to be aware of the values being of different types.

And because Haskell knows that types made with the newtype keyword can only have one constructor, it doesn't have to evaluate the value passed to the function to make sure that it conforms to the (CoolBool _) pattern because newtype types can only have one possible value constructor and one field!

--}

{-- type vs. newtype vs. data

The type keyword is for making type synonyms.

type IntList = [Int]

All this does is to allow us to refer to the [Int] type as IntList.

They can be used interchangeably. We don't get an IntList value constructor or anything like that.

The newtype keyword is for taking existing types and wrapping them in new types, mostly so that it's easier to make them instances of certain type classes.

newtype CharList = CharList { getCharList :: [Char] }

We can't use ++ to put together a CharList and a list of type [Char]. We can't even use ++ to put together two CharLists, because ++ works only on lists and the CharList type isn't a list, even though it could be said that it contains one. We can, however, convert two CharLists to lists, ++ them and then convert that back to a CharList.

When we use record syntax in our newtype declarations, we get functions for converting between the new type and the original type: namely the value constructor of our newtype and the function for extracting the value in its field.

The new type also isn't automatically made an instance of the type classes that the original type belongs to, so we have to derive or manually write them.

In practice, you can think of newtype declarations as data declarations that can only have one constructor and one field.

If you catch yourself writing such a data declaration, consider using newtype.

--}
