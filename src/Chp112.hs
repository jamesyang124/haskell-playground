{-- The newtype keyword

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
