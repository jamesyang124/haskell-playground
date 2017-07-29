module Chp94
where

{-- cmd line arguments

The System.Environment module has two cool I/O actions. One is getArgs, which has a type of getArgs :: IO [String]

getProgName has a type of getProgName :: IO String and is an I/O action that contains the program name.

--}

import System.Environment
import Data.List
import System.Directory
import System.IO
import System.Random
import Control.Monad(when)

main1 = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The program name is:"
   putStrLn progName

{--

$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test

--}

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main2 = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

{--

First, we get the arguments and bind them to (command:args). If you remember your pattern matching, this means that the first argument will get bound to command and the rest of them will get bound to args.

lookup :: Eq a => a -> [(a, b)] -> Maybe b

lookup a key in Map.

--}

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

{--

random :: (RandomGen g, Random a) => g -> (a, g)

The System.Random module exports a cool type, namely StdGen that is an instance of the RandomGen typeclass. We can either make a StdGen manually or we can tell the system to give us one based on a multitude of sort of random stuff.

To manually make a random generator, use the mkStdGen function. It has a type of mkStdGen :: Int -> StdGen. It takes an integer and based on that, gives us a random generator.

Ah, right, the random function can return a value of any type that's part of the Random typeclass, so we have to inform Haskell what kind of type we want.

--}

res1 = random (mkStdGen 100) :: (Int, StdGen)

{--

The first component of the tuple is our number whereas the second component is a textual representation of our new random generator.

Let's make a function that simulates tossing a coin three times.

If random didn't return a new generator along with a random value, we'd have to make this function take three random generators as a parameter and then return coin tosses for each of them.

But that sounds wrong because if one generator can make a random value of type Int (which can take on a load of different values), it should be able to make three coin tosses (which can take on precisely eight combinations). So this is where random returning a new generator along with a value really comes in handy.

--}

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

{--

Well, there's a function called randoms that takes a generator and returns an infinite sequence of values based on that generator.

[-1807975507,545074951,-1015194702,-1622477312,-502893664]

--}

res2 = take 5 $ randoms (mkStdGen 11) :: [Int]

{--

Why doesn't randoms return a new generator as well as a list? We could implement the randoms function very easily like this

--}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

{--

randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g), meaning that it's kind of like random, only it takes as its first parameter a pair of values that set the lower and upper bounds and the final value produced will be within those bounds.

--}

res3 = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

{--

:t take
take :: Int -> [a] -> [a]

So we need to explicitly define return type as [Char]

Well, so far we've always made our random number generator manually by making it with some arbitrary integer.

The problem is, if we do that in our real programs, they will always return the same random numbers, which is no good for us.

ex:

Prelude System.Random> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
Prelude System.Random> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
Prelude System.Random> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
Prelude System.Random> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)

That's why System.Random offers the getStdGen I/O action, which has a type of IO StdGen. When your program starts, it asks the system for a good random number generator and stores that in a so called global generator.

getStdGen fetches you that global random generator when you bind it to something.

--}

main3 = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a','z') gen)

{--

Be careful though, just performing getStdGen twice will ask the system for the same global generator twice. If you do this:

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen2 <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen2)

you will get the same string printed out twice! One way to get two different strings of length 20 is to set up an infinite stream and then take the first 20 characters and print them out in one line and then take the second set of 20 characters and print them out in the second line.

For this, we can use the splitAt function from Data.List

--}

main4 = do
    gen <- getStdGen
    let randomChars = randomRs ('a','z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStr second20

{--

Another way is to use the newStdGen action, which splits our current random generator into two generators. It updates the global random generator with one of them and encapsulates the other as its result.

--}

main5 = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen')

    import System.Random
    import Control.Monad(when)

main6 = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main
