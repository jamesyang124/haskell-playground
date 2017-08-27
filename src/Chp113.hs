{-- Monoid

A monoid is when you have an "associative binary function and a value which acts as an identity with respect to that function".

When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value. 1 is the identity with respect to * and [] is the identity with respect to ++.

There are a lot of other monoids to be found in the world of Haskell, which is why the Monoid type class exists. It's for types which can act like monoids. Let's see how the type class is defined:

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
--}

{--

we see that only concrete types can be made instances of Monoid, because the m in the type class definition doesn't take any type parameters.

This is different from Functor and Applicative, which require their instances to be type constructors which take one parameter.

The first function is mempty. It's not really a function, since it doesn't take parameters, so it's a polymorphic constant, kind of like minBound from Bounded.

mempty represents the identity value for a particular monoid.

we have mappend, which, as you've probably guessed, is the binary function. It takes two values of the same type and returns a value of that type as well.

When we meet other instances of Monoid, we'll see that most of them don't append values either, so avoid thinking in terms of appending and just think in terms of mappend being a binary function that takes two monoid values and returns a third.

mconcat - it takes a list of monoid values and reduces them to a single value by doing mappend between the list's elements.

It has a default implementation, which just takes mempty as a starting value and folds the list from the right with mappend.

Because the default implementation is fine for most instances, we won't concern ourselves with mconcat too much from now on.

When making a type an instance of Monoid, it suffices to just implement mempty and mappend.

The reason mconcat is there at all is because for some instances, there might be a more efficient way to implement mconcat, but for most instances the default implementation is just fine.

--}

{-- instance should apply monoid laws to typeclass Monoid

Before moving on to specific instances of Monoid, let's take a brief look at the monoid laws.

We mentioned that there has to be a value that acts as the identity with respect to the binary function and that the binary function has to be associative.

It's possible to make instances of Monoid that don't follow these rules, but such instances are of no use to anyone because when using the Monoid type class, we rely on its instances acting like monoids.

Otherwise, what's the point? That's why when making instances, we have to make sure they follow these laws:

1. mempty `mappend` x = x
2. x `mappend` mempty = x
3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

The first two state that mempty has to act as the identity with respect to mappend and the third says that mappend has to be associative.

i.e. that it the order in which we use mappend to reduce several monoid values into one doesn't matter.

Haskell doesn't enforce these laws, so we as the programmer have to be careful that our instances do indeed obey them.

So the typeclass only define behavior, but its the duty of instance to follow the monoid laws.

--}

{-- List are monoids

instance Monoid [a] where
    mempty = []
    mappend = (++)

ghci> mempty :: [a]
[]

Notice that in the last line, we had to write an explicit type annotation, because if we just did mempty, GHCi wouldn't know which instance to use, so we had to say we want the list instance.

Because mconcat has a default implementation, we get it for free when we make something an instance of Monoid.

In the case of the list, mconcat turns out to be just concat. It takes a list of lists and flattens it, because that's the equivalent of doing ++ between all the adjecent lists in a list.

concat [[1,2,3,4], [5,6,7]]
-- [1,2,3,4,5,6,7]

Notice that monoids don't require that a `mappend` b be equal to b `mappend` a. In the case of the list, they clearly aren't:

ghci> "one" `mappend` "two"
"onetwo"
ghci> "two" `mappend` "one"
"twoone"

And that's okay. The fact that for multiplication 3 * 5 and 5 * 3 are the same is just a property of multiplication, but it doesn't hold for all (and indeed, most) monoids.

--}

{-- Product and Sum

ghci> 0 + 4
4
ghci> 5 + 0
5
ghci> (1 + 3) + 5
9
ghci> 1 + (3 + 5)
9

The monoid laws hold, because if you add 0 to any number, the result is that number. And addition is also associative.

Remember, when there are several ways for some type to be an instance of the same type class, we can wrap that type in a newtype and then make the new type an instance of the type class in a different way. We can have our cake and eat it too.

The Data.Monoid module exports two types for this, namely Product and Sum. Product is defined like this:

newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

Simple, just a newtype wrapper with one type parameter along with some derived instances. Its instance for Monoid goes a little something like this:

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

mempty is just 1 wrapped in a Product constructor. mappend pattern matches on the Product constructor, multiplies the two numbers and then wraps the resulting number back.

As you can see, there's a Num a class constraint. So this means that Product a is an instance of Monoid for all a's that are already an instance of Num.

To use Producta a as a monoid, we have to do some newtype wrapping and unwrapping:

ghci> getProduct $ Product 3 `mappend` Product 9
27
ghci> getProduct $ Product 3 `mappend` mempty
3
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
24
ghci> getProduct . mconcat . map Product $ [3,4,2]
24

Sum is defined like Product and the instance is similar as well. We use it in the same way:

ghci> getSum $ Sum 2 `mappend` Sum 9
11
ghci> getSum $ mempty `mappend` Sum 3
3
ghci> getSum . mconcat . map Sum $ [1,2,3]
6

--}
