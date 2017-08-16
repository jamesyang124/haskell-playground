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

{--

Another instance of Functor that we've been dealing with all along but didn't know was a Functor is (->) r. You're probably slightly confused now, since what the heck does (->) r mean?

The function type r -> a can be rewritten as (->) r a, much like we can write 2 + 3 as (+) 2 3. When we look at it as (->) r a, we can see (->) in a slighty different light, because we see that it's just a type constructor that takes two type parameters, just like Either.

But remember, we said that a type constructor has to take exactly one type parameter so that it can be made an instance of Functor.

That's why we can't make (->) an instance of Functor, but if we partially apply it to (->) r, it doesn't pose any problems.

If the syntax allowed for type constructors to be partially applied with sections (like we can partially apply + by doing (2+), which is the same as (+) 2), you could write (->) r as (r ->). How are functions functors? Well, let's take a look at the implementation, which lies in Control.Monad.Instances

We usually mark functions that take anything and return anything as a -> b. r -> a is the same thing, we just used different letters for the type variables.

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

If the syntax allowed for it, it could have been written as

instance Functor (r ->) where
    fmap f g = (\x -> f (g x))

But it doesn't, so we have to write it in the former fashion.

First of all, let's think about fmap's type. It's fmap :: (a -> b) -> f a -> f b. Now what we'll do is mentally replace all the f's, which are the role that our functor instance plays, with (->) r's.

We'll do that to see how fmap should behave for this particular instance.

We get fmap :: (a -> b) -> ((->) r a) -> ((->) r b).

Now what we can do is write the (->) r a and (-> r b) types as infix r -> a and r -> b, like we normally do with functions.

What we get now is fmap :: (a -> b) -> (r -> a) -> (r -> b).

We pipe the output of r -> a into the input of a -> b to get a function r -> b, which is exactly what function composition is about.

If you look at how the instance is defined above, you'll see that it's just function composition. Another way to write this instance would be:

instance Functor ((->) r) where
    fmap = (.)

ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (*100) 1
"300"

Now we can see how fmap acts just like . for functions.

The fact that fmap is function composition when used on functions isn't so terribly useful right now, but at least it's very interesting.

It also bends our minds a bit and let us see how things that act more like computations than boxes (IO and (->) r) can be functors.

The function being mapped over a computation results in the same computation but the result of that computation is modified with the function.

https://stackoverflow.com/questions/9136421/where-can-i-read-up-on-the-haskell-operator

(->) is often called the "function arrow" or "function type constructor", and while it does have some special syntax, there's not that much special about it.

It's essentially an infix "type" constructor. Give it two types, and it gives you the type of functions between those types.

The infix style of this type constructor is not part of the Haskell standard, that's why (r ->) is not allowed but ((->) r) is fine in above instance definition.

we can say (->) is a higher-kinded type. Maybe is a higher-kinded type, but (Maybe a) is a concrete type. Maybe is a type constructor, as well as type functions.

type is a set of values, typeclass is a set of types. Functor typeclass is a set of higher-kinded types which kind signature is * -> *. So partial applied higher-kinded types maybe in the set of Functor typeclass. As long as it has the same kind signature * -> *.

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

map function f over x in same context(here is list [])

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

{--

Another instance of Applicative that we've already encountered is IO. This is how the instance is implemented:


instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

Since pure is all about putting a value in a minimal context that still holds it as its result, it makes sense that pure is just return, because return does exactly that; it makes an I/O action that doesn't do anything, it just yields some value as its result, but it doesn't really do any I/O operations like printing to the terminal or reading from a file.

--}

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main1 = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a

{--

Another instance of Applicative is (->) r, so functions. They are rarely used with the applicative style outside of code golf, but they're still interesting as applicatives, so let's take a look at how the function instance is implemented.

instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

When we wrap a value into an applicative functor with pure, the result it yields always has to be that value. A minimal default context that still yields that value as a result.

That's why in the function instance implementation, pure takes a value and creates a function that ignores its parameter and always returns that value.

If we look at the type for pure, but specialized for the (->) r instance:

it's pure :: a -> (r -> a).

ghci> (pure 3) "blah"
3

Because of currying, function application is left-associative, so we can omit the parentheses.

ghci> pure 3 "blah"
3

ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508

 When we do (+) <$> (+3) <*> (*100), we're making a function that will use + on the results of (+3) and (*100) and return that.

 To demonstrate on a real example, when we did (+) <$> (+3) <*> (*100) $ 5, the 5 first got applied to (+3) and (*100), resulting in 8 and 500. Then, + gets called with 8 and 500, resulting in 508.

--}

{-- review of ((->) r) in <*>, <$>

we know fmap f g :: (a -> b) -> z a -> z b

so f :: (a ->b), g :: z a

and in ((->) r) higer-kinded type this is defined as:

z a :: ((->) r) a

fmap f g :: (a -> b) -> ((->) r) a -> ((->) r) b as infix:
            (a -> b) -> (r -> a) -> (r -> b)

Note that f a must be concrete type! so it is ((->) r) a
Then rewrite it as infix operator: r -> a

<$> :: (functor f) => (a -> b) -> f a -> f b

f <$> g = fmap f g

so output of <$> for ((->) r) is as:

((->) r) b which is equivalent to ((->) r) (f (g r)) , rewrite it to lambda:
r -> (f a) == r -> z b

then:

fmap f g = \r -> f (g r)  or as \x -> f (g x)

<*> :: f (a -> b) -> f a -> f b  the same as:

((-> r) a -> b) -> ((-> r) a) -> ((-> r) b) ==
(r -> (a -> b)) -> (r -> a) -> (r -> b)

f <*> g => then we know f r == (a -> b) so

f <*> g = \x -> f x (g x), since here x or r is the same type

In here we can fnd that f should be a "binary" higer-kinded type, ex: (+), (-)
In here we can fnd that g should be a "unary" higer-kinded type, ex: (+ 3), (- 2)

so (+) <$> (+3) <*> (*100) $ 5

=> fmap (+) (+3) ...
=> (\x -> (+ ((+) 3 x))) <*> (* 100) ...
    ^ as z
=> (\r -> z r (* 100 r)) $ 5
=> z 5 (* 100 5)
=> z 5 (500)
=> (+ ((+) 3 5)) 500
=> (+ 8) 500
=> 508

pure f <*> x <*> y ... == fmap f x <*> y ... == f <$> x <*> y ...

--}
