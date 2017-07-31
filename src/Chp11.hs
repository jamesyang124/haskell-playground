-- Skip Chp10 - Functionally Solving Problems

{-- Functor

Many times the box analogy is used to help you get some intuition for how functors work, and later, we'll probably use the same analogy for applicative functors and monads.

A more correct term for what a functor is would be "computational context". The context might be that the computation can have a value or it might have failed (Maybe and Either a) or that there might be more values (lists), stuff like that.

When we first learned about curried functions, we said that all Haskell functions actually take one parameter. A function a -> b -> c actually takes just one parameter of type a and then returns a function b -> c

So a -> b -> c can be written as a -> (b -> c), to make the currying more apparent.

In the same vein, if we write fmap :: (a -> b) -> (f a -> f b), we can think it as take a function, and another function which takes Functor a and returns Functor as result.

It takes an a -> b function and returns a function f a -> f b. This is called lifting a function.

:t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a

The expression fmap (*2) is a function that takes a functor f over numbers and returns a functor over numbers.

This is even more apparent if we partially apply, say, fmap (++"!") and then bind it to a name in GHCI.

--}

{--

You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor, or you can think of it as a function that takes a function and lifts that function so that it operates on functors(lifting).

--}

{-- Functor laws

we're going to look at the functor laws. In order for something to be a functor, it should satisfy some laws. All functors are expected to exhibit certain kinds of functor-like properties and behaviors:

1. If we map the id function over a functor, the functor that we get back should be the same as the original functor. Remember, id is the identity function, which just returns its parameter unmodified.

fmap id = id

2. Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.

fmap f . fmap g = fmap (f . g)
or for any functor F

fmap f (fmap g F) = fmap (f . g) F

--}

{--

Let's take a look at a pathological example of a type constructor being an instance of the Functor typeclass but not really being a functor, because it doesn't satisfy the laws. Let's say that we have a type:

--}

data CMaybe a = CNothing | CJust Int a deriving (Show)
-- The C here stands for counter.

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

{--

we can even play with this a bit:

ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
ghci> fmap (++"blah") CNothing
CNothing

Does this obey the functor laws? In order to see that something doesn't obey a law, it's enough to find just one counter-example:

ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"

When we use a functor, it shouldn't matter if we first compose a few functions and then map them over the functor or if we just map each function over a functor in succession. But with CMaybe, it matters, because it keeps track of how many times it's been mapped over.

If we wanted CMaybe to obey the functor laws, we'd have to make it so that the Int field stays the same when we use fmap.

--}

{--

At first, the functor laws might seem a bit confusing and unnecessary, but then we see that if we know that a type obeys both laws, we can make certain assumptions about how it will act.

If a type obeys the functor laws, we know that calling fmap on a value of that type will only map the function over it, nothing more.

This leads to code that is more abstract and extensible, because we can use laws to reason about behaviors that any functor should have and make functions that operate reliably on any functor.

--}

{-- Applicative Functor

But what if we have a functor value of Just (3 *) and a functor value of Just 5 and we want to take out the function from Just (3 *) and map it over Just 5? With normal functors, we're out of luck, because all they support is just mapping normal functions over existing functors.

But we can't map a function that's inside a functor over another functor with what fmap offers us. We could pattern-match against the Just constructor to get the function out of it and then map it over Just 5, but we're looking for a more general and abstract way of doing that, which works across functors.

Meet the Applicative typeclass. It lies in the Control.Applicative module and it defines two methods, pure and <*>.

It doesn't provide a default implementation for any of them, so we have to define them both if we want something to be an applicative functor. The class is defined like so:

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

A better way of thinking about pure would be to say that it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that value.

Whereas fmap takes a function and a functor and applies the function inside the functor, <*> takes a functor that has a function in it and another functor and sort of extracts that function from the first functor and then maps it over the second one.

When I say extract, I actually sort of mean run and then extract, maybe even sequence. We'll see why soon.

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

ghci> Just (++"hahah") <*> Nothing
Nothing
ghci> Nothing <*> Just "woot"
Nothing

The first four input lines demonstrate how the function is extracted and then mapped, but in this case, they could have been achieved by just mapping unwrapped functions over functors.

The last line is interesting, because we try to extract a function from a Nothing and then map it over something, which of course results in a Nothing.

--}

{--

With normal functors, you can just map a function over a functor and then you can't get the result out in any general way, even if the result is a partially applied function.

Applicative functors, on the other hand, allow you to operate on several functors with a single function. Check out this piece of code:

ghci> pure (+) <*> Just 3 <*> Just 5
Just 8
ghci> pure (+) <*> Just 3 <*> Nothing
Nothing
ghci> pure (+) <*> Nothing <*> Just 5
Nothing

<*> is left-associative, which means that pure (+) <*> Just 3 <*> Just 5 is the same as (pure (+) <*> Just 3) <*> Just 5

(pure (+) <*> Just 3) <*> Just 5
=> (Just (+) <*> Just 3) <*> Just 5
=> Just (3+) <*> Just 5
=> fmap (3+) Just 5
=> Just (3+5)
=> Just 8

Applicative functors and the applicative style of doing pure f <*> x <*> y <*> ... allow us to take a function that expects parameters that aren't necessarily wrapped in functors and use that function to operate on several values that are in functor contexts.

The function can take as many parameters as we want, because it's always partially applied step by step between occurences of <*>

This becomes even more handy and apparent if we consider the fact that pure f <*> x equals fmap f x. This is one of the applicative laws.

Instead of writing pure f <*> x <*> y <*> ..., we can write fmap f x <*> y <*> ....

This is why Control.Applicative exports a function called <$>, which is just fmap as an infix operator. Here's how it's defined:

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

Quick reminder:

type variables are independent of parameter names or other value names.

The f in the function declaration here is a type variable with a class constraint saying that any type constructor that replaces f should be in the Functor typeclass.

The f in the function body denotes a function that we map over x.

The fact that we used f to represent both of those doesn't mean that they somehow represent the same thing.

--}

{--

By using <$>, the applicative style really shines, because now if we want to apply a function f between three applicative functors, we can write f <$> x <*> y <*> z.

If the parameters weren't applicative functors but normal values, we'd write f x y z.

--}

{--

Lists (actually the list type constructor, []) are applicative functors.

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]


Earlier, we said that pure takes a value and puts it in a default context. Or in other words, a minimal context that still yields that value.

The minimal context for lists would be the empty list, [], but the empty list represents the lack of a value, so it can't hold in itself the value that we used pure on. That's why pure takes a value and puts it in a singleton list.

Similarly, the minimal context for the Maybe applicative functor would be a Nothing, but it represents the lack of a value instead of a value, so pure is implemented as Just in the instance implementation for Maybe.

ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]

[(+),(*)] <*> [1,2] <*> [3,4]
[(1+),(2+),(1*),(2*)] <*> [3,4]
-- because every function on the left gets applied to every value on the right.
[4,5,5,6,3,4,6,8]

--}
