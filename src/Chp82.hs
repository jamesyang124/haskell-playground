module Chp82
where

{-- Derived instances --}

{--

We explained that a typeclass is a sort of an interface that defines some behavior. A type can be made an instance of a typeclass if it supports that behavior.

--}


{--

We also mentioned that they're often confused with classes in languages like Java, Python, C++ and the like, which then baffles a lot of people. In those languages, classes are a blueprint from which we then create objects that contain state and can do some actions.

Typeclasses are more like interfaces. We don't make data from typeclasses. Instead, we first make our data type and then we think about what it can act like.

If it can act like something that can be equated, we make it an instance of the Eq typeclass.

--}

{--

Haskell can derive the behavior of our types in these contexts if we use the deriving keyword when making our data type.

--}

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)

{--

When we derive the Eq instance for a type and then try to compare two values of that type with == or /=, Haskell will see if the value constructors match (there's only one value constructor here though) and then it will check if all the data contained inside matches by testing each pair of fields with ==.

There's only one catch though, the types of all the fields also have to be part of the Eq typeclass.

But since both String and Int are, we're OK. Let's test our Eq instance.

--}

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

{--

mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
true

we can use it as the "a" for all functions that have a class constraint of "Eq a" in their type signature, such as "elem".

The Show and Read typeclasses are for things that can be converted to or from strings, respectively. Like with Eq, if a type's constructors have fields, their type has to be a part of Show or Read if we want to make our type an instance of them.

--}

data Person2 = Person2 { firstName2 :: String
                     , lastName2 :: String
                     , age2 :: Int
                     } deriving (Eq, Show, Read)

{--

`Read` is pretty much the inverse typeclass of `Show`. `Show` is for converting values of our a type to a string, `Read` is for converting strings to values of our type.

Remember though, when we use the `read` function, we have to use an explicit type annotation to tell Haskell which type we want to get as a result.

If we don't make the type we want as a result explicit, Haskell doesn't know which type we want.

--}

res3 = read "Person2 {firstName2 =\"Michael\", lastName2 =\"Diamond\", age2 = 43}" :: Person2

{--

If we use the result of our read later on in a way that Haskell can infer that it should read it as a person, we don't have to use type annotation.

read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
True

We can also read parameterized types, but we have to fill in the type parameters.

So we can't do:
read "Just 't'" :: Maybe a

but we can do:
read "Just 't'" :: Maybe Char
--}

{--

We can derive instances for the `Ord` type class, which is for types that have values that can be ordered.

If we compare two values of the same type that were made using different constructors, the value which was made with a constructor that's defined first is considered smaller.

data Bool = False | True deriving (Ord)

--}

res4 = True `compare` False

res5 = True > False

{--

In the `Maybe a` data type, the `Nothing` value constructor is specified before the Just value constructor, so a value of `Nothing` is always smaller than a value of Just something, even if that something is minus one billion trillion.

But if we compare two Just values, then it goes to compare what's inside them.

But we can't do something like Just (*3) > Just (*2), because (*3) and (*2) are functions, which aren't instances of Ord.

--}

data Day1 = Monday1 | Tuesday1 | Wednesday1 | Thursday1 | Friday1 | Saturday1 | Sunday1

{--

Because all the value constructors are nullary (take no parameters, i.e. fields), we can make it part of the Enum typeclass. The Enum typeclass is for things that have predecessors and successors.

We can also make it part of the Bounded typeclass, which is for things that have a lowest possible value and highest possible value.

--}

data Day2 = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

res6 = minBound :: Day2
-- Monday

{--

Type synonyms

Type synonyms don't really do anything per se, they're just about giving some types different names so that they make more sense to someone reading our code and documentation.

Here's how the standard library defines String as a synonym for [Char].

--}

type String2 = [Char]

{--

Type synonyms can also be parameterized. If we want a type that represents an association list type but still want it to be general so it can use any type as the keys and values, we can do this:

--}

type AssocList k v = [(k,v)]

{--

Now, a function that gets the value by a key in an association list can have a type of (Eq k) => k -> AssocList k v -> Maybe v.

AssocList is a type constructor that takes two types and produces a concrete type, like AssocList Int String, for instance.

When I talk about concrete types I mean like fully applied types like Map Int String or if we're dealin' with one of them polymorphic functions, [a] or (Ord a) => Maybe a and stuff.

And like, sometimes me and the boys say that Maybe is a type, but we don't mean that, cause every idiot knows Maybe is a type constructor.

When I apply an extra type to Maybe, like "Maybe String", then I have a concrete type. You know, values can only have types that are concrete types!

--}

{--

Partial applied type constructor

Just like we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them.

Just like we call a function with too few parameters to get back a new function, we can specify a type constructor with too few type parameters and get back a partially applied type constructor.

type IntMap v = Map Int v

or we can do this

type IntMap = Map Int

When you do a qualified import, type constructors also have to be preceeded with a module name. So you'd write type IntMap = Map.Map Int.

Make sure that you really understand the distinction between type constructors and value constructors.

Just because we made a type synonym called `IntMap` or `AssocList` doesn't mean that we can do stuff like:

AssocList [(1,2),(4,5),(7,9)]

All it means is that we can refer to its type by using different names.
We can do:

[(1,2),(3,5),(8,9)] :: AssocList Int Int

which will make the numbers inside assume a type of Int.

Type synonyms (and types generally) can only be used in the type portion of Haskell.

We're in Haskell's type portion whenever we're defining new types (so in data and type declarations) or when we're located after a "::". The "::" is in type declarations or in type annotations.

--}

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

{--

Simple stuff. We introduce a new data type to represent whether a locker is taken or free and we make a type synonym for the locker code.

--}
