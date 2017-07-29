module Chp84
where

{-- Functor typeclass --}

{--

And now, we're going to take a look at the Functor typeclass, which is basically for things that can be mapped over.

You're probably thinking about lists now, since mapping over lists is such a dominant idiom in Haskell. And you're right, the list type is part of the Functor typeclass.

class Functor f where
    fmap :: (a -> b) -> f a -> f b

quick refresher example: Maybe Int is a concrete type, but Maybe is a type constructor that takes one type as the parameter.


Anyway, we see that fmap takes a function from one type to another, a functor applied with one type, and returns a functor applied with another type.

--}

{--

this type declaration for fmap reminds me of something. If you don't know what the type signature of map is, it's: map :: (a -> b) -> [a] -> [b]

map is just a fmap that works only on lists. Here's how the list is an instance of the Functor typeclass:

instance Functor [] where
    fmap = map

--}

{--

What happens when we map or fmap over an empty list? Well, of course, we get an empty list.

It just turns an empty list of type [a] into an empty list of type [b].

--}

{--

Types that can act like a box can be functors.

You can think of a list as a box that has an infinite amount of little compartments and they can all be empty, one can be full and the others empty or a number of them can be full.

So, what else has the properties of being like a box? For one, the "Maybe a" type.

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

--}

res1 = fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")

{--

Again, notice how we wrote instance Functor Maybe where instead of instance Functor (Maybe m) where, like we did when we were dealing with Maybe and YesNo. Then Functor wants a type constructor that takes one type and not a concrete type.

class Functor f where
    fmap :: (a -> b) -> f a -> f b

If you mentally replace the f to Maybe, fmap acts like a (a -> b) -> Maybe a -> Maybe b for this particular type, which looks OK.

But if you replace f with (Maybe m), then it would seem to act like a (a -> b) -> Maybe m a -> Maybe m b, which doesn't make any damn sense because Maybe takes just one type parameter.

invalid
instance Functor (Maybe m) where
    fmap ???
    fmap ???

--}

{--

If you look at fmap as if it were a function made only for Tree, its type signature would look like (a -> b) -> Tree a -> Tree b.

We're going to use recursion on this one.

1. Mapping over an empty tree will produce an empty tree.

2. Mapping over a non-empty tree will be a tree consisting of our function applied to the root value and its left and right sub-trees will be the previous sub-trees, only our function will be mapped over them.

--}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

{--

Now how about Either a b? Can this be made a functor? The Functor typeclass wants a type constructor that takes only one type parameter but Either takes two.

Hmmm! I know, we'll partially apply Either by feeding it only one parameter so that it has one free parameter.

Here's how Either a is a functor in the standard libraries:

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

--}

{--

Functors should obey some laws so that they may have some properties that we can depend on and not think about too much.

If we use fmap (+1) over the list [1,2,3,4], we expect the result to be [2,3,4,5] and not its reverse, [5,4,3,2].

If we use fmap (\a -> a) (the identity function, which just returns its parameter) over some list, we expect to get back the same list as a result.

--}

{-- Kinds and some type-foo

Type constructors take other types as parameters to eventually produce concrete types. That kind of reminds me of functions, which take values as parameters to produce values.

We've seen that type constructors can be partially applied (Either String is a type that takes one type and produces a concrete type, like Either String Int), just like functions can.

--}

{--

So, values like 3, "YEAH" or takeWhile (functions are also values, because we can pass them around and such) each have their own type.

Types are little labels that values carry so that we can reason about the values. But types have their own little labels, called kinds.

A kind is more or less the type of a type.

let's examine the kind of a type by using the :k command in GHCI.

ghci> :k Int
Int :: *

A * means that the type is a concrete type. A concrete type is a type that doesn't take any type parameters and values can only have types that are concrete types.

ghci> :k Maybe
Maybe :: * -> *

The Maybe type constructor takes one concrete type (like Int) and then returns a concrete type like Maybe Int.

And that's what this kind tells us. Just like Int -> Int means that a function takes an Int and returns an Int, * -> * means that the type constructor takes one concrete type and returns a concrete type.

ghci> :k Maybe Int
Maybe Int :: *

We applied the type parameter to Maybe and got back a concrete type (that's what * -> * means.

A parallel (although not equivalent, types and kinds are two different things) to this is if we do :t isUpper and :t isUpper 'A'.

isUpper has a type of Char -> Bool and isUpper 'A' has a type of Bool,

We used :k on a type to get its kind, just like we can use :t on a value to get its type. Like we said, types are the labels of values and kinds are the labels of types and there are parallels between the two.

--}

{--

ghci> :k Either
Either :: * -> * -> *

It also looks kind of like a type declaration of a function that takes two values and returns something.

Type constructors are curried (just like functions), so we can partially apply them.

ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *

--}

{--

When we wanted to make Either a part of the Functor typeclass, we had to partially apply it because Functor wants types that take only one parameter while Either takes two.

In other words, Functor wants types of kind * -> * and so we had to partially apply Either to get a type of kind * -> * instead of its original kind * -> * -> *. If we look at the definition of Functor again

class Functor f where
    fmap :: (a -> b) -> f a -> f b

we see that the f type variable is used as a type that takes one concrete type to produce a concrete type. We know it has to produce a concrete type because it's used as the type of a value in a function.

And from that, we can deduce that types that want to be friends with Functor have to be of kind * -> *. (imply by f a, or f b)

--}

{--

class Tofu t where
    tofu :: j a -> t a j

Because j a is used as the type of a value that the tofu function takes as its parameter, j a has to have a kind of *

So kind:

j :: * -> *
t :: * -> (* -> *) -> *

We assume * for a and so we can infer that j has to have a kind of * -> *.

We see that t has to produce a concrete value too and that it takes two types. And knowing that a has a kind of * and j has a kind of * -> *, we infer that t has to have a kind of * -> (* -> *) -> *.

So it takes a concrete type (a), a type constructor that takes one concrete type (j), and produces a concrete type.

so let's make a type with a kind of * -> (* -> *) -> *. Here's one way of going about it.

data Frank a b  = Frank {frankField :: b a} deriving (Show)


How do we know this type has a kind of * -> (* -> *) - > *?

Well, fields in ADTs are made to hold values, so they must be of kind *, obviously.

We assume * for a, which means that b takes one type parameter and so its kind is * -> *.

Now we know the kinds of both a and b and because they're parameters for Frank, we see that Frank has a kind of * -> (* -> *) -> * The first * represents a and the (* -> *) represents b.

Let's make some Frank values and check out their types.

:t Frank {frankField = Just "HAHA"}
-- Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe

:t Frank {frankField = "YES"}
-- Frank {frankField = "YES"} :: Frank Char []

--}

{--

Making Frank an instance of Tofu is pretty simple. We see that tofu takes a "j a" (so an example type of that form would be Maybe Int) and returns a "t a j".

So if we replace Frank with j, the result type would be Frank Int Maybe.

instance Tofu Frank where
    tofu x = Frank x

ghci> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}

ghci> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}

--}

{--

Example:

data Barry t k p = Barry { yabba :: p, dabba :: t k }

ghci> :k Barry
Barry :: (* -> *) -> * -> * -> *

Now, to make this type a part of Functor we have to partially apply the first two type parameters so that we're left with * -> *.

That means that the start of the instance declaration will be: instance Functor (Barry a b) where.

If we look at fmap as if it was made specifically for Barry, it would have a type of fmap :: (a -> b) -> Barry c d a -> Barry c d b, because we just replace the Functor's "f" with "Barry c d". The third type parameter from Barry will have to change and we see that it's conviniently in its own field.

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

--}
