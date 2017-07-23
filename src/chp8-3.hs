{-- Recursive DataStructure --}

{--

As we've seen, a constructor in an algebraic data type can have several (or none at all) fields and each field must be of some concrete type. With that in mind, we can make types whose constructors have fields that are of the same type!

Using that, we can create recursive data types, where one value of some type contains values of that type, which in turn contain more values of the same type and so on.

Same goes for a list like 3:(4:(5:6:[])), which could be written either like that or like 3:4:5:6:[] (because : is right-associative) or [3,4,5,6].

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

--}

data List1 a = Empty1 | Cons { listHead :: a, listTail :: List1 a} deriving (Show, Read, Eq, Ord)

{--

You might also be confused about the "Cons" constructor here. cons is another word for ":". You see, in lists, ":" is actually a constructor that takes a value and another list and returns a list.

--}

res1 = 5 `Cons` Empty1

{--

We called our Cons constructor in an infix manner so you can see how it's just like ":".

--}

{-- define infix operator

We can define functions to be automatically infix by making them comprised of only special characters. We can also do the same with constructors, since they're just functions that return a data type.

--}

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

{--

we notice a new syntactic construct, the fixity declarations. When we define functions as operators, we can use that to give them a fixity (but we don't have to). A fixity states how tightly the operator binds and whether it's left-associative or right-associative.

For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6. That means that they're both left-associative.

but * binds tighter than +, because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3.

--}

res2 = 3 :-: 4 :-: 5 :-: Empty

{--

Let's make a function that adds two of our lists together.

--}

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

a = 3 :-: 4 :-: 5 :-: Empty
b = 6 :-: 7 :-: Empty

res3 = a .++ b

{-- pattern matching

Because pattern matching works (only) on constructors, we can match for stuff like that, normal prefix constructors or stuff like 8 or 'a', which are basically constructors for the numeric and character types, respectively.

Example of implement binary search tree:

The singleton function is just a shortcut for making a node that has something and then two empty sub-trees.

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

{--

First off, if the element we're inserting is equal to the root element, just return a tree that's the same. If it's smaller, return a tree that has the same root value, the same right sub-tree but instead of its left sub-tree, put a tree that has our value inserted into it.

--}

{-- typeclass 102

The behavior of typeclasses is achieved by defining functions or just type declarations that we then implement. So when we say that a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with that type.

--}

class Eq2 a where
    (.==) :: a -> a -> Bool
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)
    x ./= y = not (x .== y)

{--

when we write class Eq a where, this means that we're defining a new typeclass and that's called Eq.

The a is the type variable and it means that a will play the role of the type that we will soon be making an instance of Eq. It doesn't have to be called a, it doesn't even have to be one letter, it just has to be a lowercase word.

If we have say class Eq a where and then define a type declaration within that class like (==) :: a -> -a -> Bool, then when we examine the type of that function later on, it will have the type of (Eq a) => a -> a -> Bool.

--}

data TrafficLight = Red | Yellow | Green

{--

It defines the states of a traffic light. Even though we could derive them for types like Eq and Show. Here's how we make it an instance of Eq.

--}

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

{--

So class is for defining new typeclasses and instance is for making our types instances of typeclasses.

we wrote class Eq a where and we said that a plays the role of whichever type will be made an instance later on.

Because when we're making an instance, we write instance Eq TrafficLight where. We replace the a with the actual type.

--}

{-- minimal complete definition for the typeclass

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

Because == was defined in terms of /= and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration.

That's called the minimal complete definition for the typeclass — the minimum of functions that we have to implement so that our type can behave like the class advertises.

To fulfill the minimal complete definition for Eq, we have to overwrite either one of == or /=.

If Eq was defined simply like this:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

without x == y = not (x /= y)
or      x /= y = not (x == y)

Then we'd have to implement both of these functions when making a type an instance of it, because Haskell wouldn't know how these two functions are related. The minimal complete definition would then be: both == and /=.


Let's make this an instance of Show by hand, too. To satisfy the minimal complete definition for Show, we just have to implement its show function, which takes a value and turns it into a string.

--}

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

{--

You can also make typeclasses that are subclasses of other typeclasses. The class declaration for Num is a bit long, but here's the first part:

class (Eq a) => Num a where

So this is just like writing class Num a where, only we state that our type a must be an instance of Eq. We're essentially saying that we have to make a type an instance of Eq before we can make it an instance of Num.

That's all there is to subclassing really, it's just a class constraint on a class declaration!

--}

{--

From the type declarations, we see that the a is used as a concrete type because all the types in functions have to be concrete (remember, you can't have a function of the type a -> Maybe but you can have a function of a -> Maybe a or Maybe Int -> Maybe String)

Because like we've seen, the a has to be a concrete type but Maybe isn't a concrete type. It's a type constructor that takes one parameter and then produces a concrete type. It would also be tedious to write instance Eq (Maybe Int) where, instance Eq (Maybe Char) where, etc. for every type ever. So we could write it out like so:

instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

By specifying a type parameter (m, which is in lowercase), we said that we want all types that are in the form of Maybe m, where m is any type, to be an instance of Eq.

There's one problem with this though. We use == on the contents of the Maybe but we have no assurance that what the Maybe contains can be used with Eq!

That's why we have to modify our instance declaration like this:

instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

We had to add a class constraint! With this instance declaration, we say this: we want all types of the form Maybe m to be part of the Eq typeclass, but only those types where the m (so what's contained inside the Maybe) is also a part of Eq.

This is actually how Haskell would derive the instance too.

Most of the times, class constraints in class declarations are used for making a typeclass a subclass of another typeclass and class constraints in instance declarations are used to express requirements about the contents of some type.

--}

{--

(==) :: Maybe -> Maybe -> Bool doesn't make much sense but (==) :: (Eq m) => Maybe m -> Maybe m -> Bool does. But this is just something to think about, because == will always have a type of (==) :: (Eq a) => a -> a -> Bool, no matter what instances we make.

If you want to see what the instances of a typeclass are, just do :info YourTypeClass in GHCI.

--}

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True


res4 = yesno $ Just 0
-- True

res5 = yesno True
-- True

res6 = yesno EmptyTree
-- False
