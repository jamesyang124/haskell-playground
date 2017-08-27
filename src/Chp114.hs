{-- Any and All

Another type which can act like a monoid in two distinct but equally valid ways is Bool. The first way is to have the or function || act as the binary function along with False as the identity value.

The way or works in logic is that if any of its two parameters is True, it returns True, otherwise it returns False.

The Any newtype constructor is an instance of Monoid in this fashion. It's defined like this:

newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)


instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)

This Any type does not take any type parameter, so it is a concrete type as well.

The reason it's called Any is because x `mappend` y will be True if any one of those two is True.

Even if three or more Any wrapped Bools are mappended together, the result will hold True if any of them are True:

ghci> getAny $ Any True `mappend` Any False
True
ghci> getAny $ mempty `mappend` Any True
True
ghci> getAny . mconcat . map Any $ [False, False, False, True]
True
ghci> getAny $ mempty `mappend` mempty
False

--}

{-- All

The other way for Bool to be an instance of Monoid is to kind of do the opposite: have && be the binary function and then make True the identity value.

newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)


getAll . mconcat . map All $ [True, True, False]
-- False

--}

{-- The Ordering monoid

Hey, remember the Ordering type? It's used as the result when comparing things and it can have three values: LT, EQ and GT, which stand for less than, equal and greater than respectively:

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

The instance is set up like this: when we mappend two Ordering values, the one on the left is kept, unless the value on the left is EQ, in which case the right one is the result. The identity is EQ. At first, this may seem kind of arbitrary

It's important to note that in the Monoid instance for Ordering, x `mappend` y doesn't equal y `mappend` x. Because the first parameter is kept unless it's EQ

LT `mappend` GT will result in LT, whereas GT `mappend` LT will result in GT:

ghci> LT `mappend` GT
LT
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
ghci> mempty `mappend` GT
GT

OK, so how is this monoid useful? Let's say you were writing a function that takes two strings, compares their lengths, and returns an Ordering.

But if the strings are of the same length, then instead of returning EQ right away, we want to compare them alphabetically. One way to write this would be like so:

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a

But by employing our understanding of how Ordering is a monoid, we can rewrite this function in a much simpler manner:

import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT

when we use mappend, its left parameter is always kept unless it's EQ, in which case the right one is kept. That's why we put the comparison that we consider to be the first, more important criterion as the first parameter.

If we wanted to expand this function to also compare for the number of vowels and set this to be the second most important criterion for comparison, we'd just modify it like this:

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
                    where vowels = length . filter (`elem` "aeiou")

ghci> lengthCompare "zen" "anna"
LT
ghci> lengthCompare "zen" "ana"
LT
ghci> lengthCompare "zen" "ann"
GT

The Ordering monoid is very cool because it allows us to easily compare things by many different criteria and put those criteria in an order themselves, ranging from the most important to the least.

--}

{-- Maybe the monoid

Let's take a look at the various ways that Maybe a can be made an instance of Monoid and what those instances are useful for.

One way is to treat Maybe a as a monoid only if its type parameter a is a monoid as well and then implement mappend in such a way that it uses the mappend operation of the values that are wrapped with Just.

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})

But what if the type of the contents of the Maybe aren't an instance of Monoid?

Notice that in the previous instance declaration, the only case where we have to rely on the contents being monoids is when both parameters of mappend are Just values.

But if we don't know if the contents are monoids, we can't use mappend between them, so what are we to do?

Well, one thing we can do is to just discard the second value and keep the first one. For this, the First a type exists and this is its definition:

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

Just like we said. mempty is just a Nothing wrapped with the First newtype constructor. If mappend's first parameter is a Just value, we ignore the second one.

If the first one is a Nothing, then we present the second parameter as a result, regardless of whether it's a Just or a Nothing:

ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'

First is useful when we have a bunch of Maybe values and we just want to know if any of them is a Just. The mconcat function comes in handy:

ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9

--}

{-- Last

Data.Monoid provides a the Last a type, which works like First a, only the last non-Nothing value is kept when mappending and using mconcat:

ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
Just 10
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
Just "two"

--}

{-- Using monoids to fold data structures

Because there are so many data structures that work nicely with folds, the Foldable type class was introduced.

Much like Functor is for things that can be mapped over, Foldable is for things that can be folded up! It can be found in Data.Foldable and because it export functions whose names clash with the ones from the Prelude, it's best imported qualified (and served with basil):

import qualified Foldable as F

ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

So whereas foldr takes a list and folds it up, the foldr from Data.Foldable accepts any type that can be folded up, not just lists! As expected, both foldr functions do the same for lists:

ghci> foldr (*) 1 [1,2,3]
6
ghci> F.foldr (*) 1 [1,2,3]
6

ghci> F.foldl (+) 2 (Just 9)
11
ghci> F.foldr (||) False (Just True)
True

--}

{-- foldMap

But folding over a Maybe value isn't terribly interesting, because when it comes to folding, it just acts like a list with one element if it's a Just value and as an empty list if it's Nothing.

So let's examine a data structure that's a little more complex then.

Now, we're going to make it an instance of Foldable so that we get the abilty to fold it up. One way to make a type constructor an instance of Foldable is to just directly implement foldr for it.

But another, often much easier way, is to implement the foldMap function, which is also a part of the Foldable type class. The foldMap function has the following type:

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

Its first parameter is a function that takes a value of the type that our foldable structure contains (denoted here with a) and returns a monoid value.

Its second parameter is a foldable structure that contains values of type a.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

We think like this: if we are provided with a function that takes an element of our tree and returns a monoid value, how do we reduce our whole tree down to one single monoid value?

When we were doing fmap over our tree, we applied the function that we were mapping to a node and then we recursively mapped the function over the left sub-tree as well as the right one.

Here, we're tasked with not only mapping a function, but with also joining up the results into a single monoid value by using mappend.

The case of a non-empty node is a bit more interesting. It contains two sub-trees as well as a value.

In this case, we recursively foldMap the same function f over the left and the right sub-trees. Remember, our foldMap results in a single monoid value.

--}

{--

Now that we have a Foldable instance for our tree type, we get foldr and foldl for free! Consider this tree:

testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

ghci> F.foldl (+) 0 testTree
42
ghci> F.foldl (*) 1 testTree
64800

And also, foldMap isn't only useful for making new instances of Foldable; it comes in handy for reducing our structure to a single monoid value. For instance, if we want to know if any number in our tree is equal to 3, we can do this:

ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True

We can also easily turn our tree into a list by doing a foldMap with the \x -> [x] function. By first projecting that function onto our tree, each element becomes a singleton list. The mappend action that takes place between all those singleton list results in a single list that holds all of the elements that are in our tree:

ghci> F.foldMap (\x -> [x]) testTree
[1,3,6,5,8,9,10]  

--}
