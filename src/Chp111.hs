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

{--

by type signature. we might have different impl for [], ex:

[(+3),(*2)] <*> [1,2] could result to [4,5,2,4] or [1 + 3, 2 * 2]

In order to distinguish this, haskell provide a ZipList applicative functor:

instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

for all cases like [1 + 3, 2 * 2]

So how do zip lists work in an applicative style?

Let's see. Oh, the ZipList a type doesn't have a Show instance, so we have to use the "getZipList" function to extract a raw list out of a zip list.

--}

getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]

{--

The (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y).

Control.Applicative defines a function that's called liftA2, which has a type of

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c

It's defined like this:

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

It's also interesting to look at this function's type as:

(a -> b -> c) -> (f a -> f b -> f c)

When we look at it like this, we can say that liftA2 takes a normal binary function and promotes it to a function that operates on two functors.

ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]

It seems that we can combine any amount of applicatives into one applicative that has a list of the results of those applicatives inside it.

Let's try implementing a function that takes a list of applicatives and returns an applicative that has a list as its result value. We'll call it sequenceA.

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

Another way to implement sequenceA is with a fold. Remember, pretty much any function where we go over a list element by element and accumulate a result along the way can be implemented with a fold.

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]

sequenceA [[1], []]
=> (:) <$> [1] <*> sequenceA [[]]
=> (:) <$> [1] <*> ((:) <$> [] <*> sequenceA [])
=> (:) <$> [1] <*> ((:) <$> [] <*> [[]])
=> (:) <$> [1] <*> []
=> [((:) 1)] <*> []
=> fmap ((:) 1) []
=> by defintion we know fmap f [] = []
=> []
=> or
=> [f x | f <- ((:) 1), x <- []]  by fs <*> xs = [f x | f <- fs, x <- xs]
=> or
=> do f <- fs
      x <- xs
      f x


=> fs >>= (\f -> xs >>= (\x -> f x))
=> since [] >>= (\x -> [(+ 1) x]) = []
=> fs >>= (\f -> [])
=> and xs >>= k = join (fmap k xs)
=> join (fmap (\f -> []) fs)
=> []

instance Functor [] where
  fmap = map

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Base.html#fmap

map _ []     = []
map f (x:xs) = f x : map f xs

-- http://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Base.html#map

--}

{--

ghci> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True

ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]
ghci> and $ sequenceA [(>4),(<10),odd] 7
True 

--}
