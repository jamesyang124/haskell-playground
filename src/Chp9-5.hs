import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

{-- Bytestring

However, processing files as strings has one drawback: it tends to be slow. As you know, String is a type synonym for [Char]. Chars don't have a fixed size, because it takes several bytes to represent a character from, say, Unicode. Furthemore, lists are really lazy.

If you have a list like [1,2,3,4], it will be evaluated only when completely necessary. So the whole list is sort of a promise of a list. Remember that [1,2,3,4] is syntactic sugar for 1:2:3:4:[]. When the first element of the list is forcibly evaluated (say by printing it), the rest of the list 2:3:4:[] is still just a promise of a list, and so on.

So you can think of lists as promises that the next element will be delivered once it really has to and along with it, the promise of the element after it. It doesn't take a big mental leap to conclude that processing a simple list of numbers as a series of promises might not be the most efficient thing in the world.

That overhead doesn't bother us so much most of the time, but it turns out to be a liability when reading big files and manipulating them. That's why Haskell has bytestrings. Bytestrings are sort of like lists, only each element is one byte (or 8 bits) in size. The way they handle laziness is also different.

--}

{--

Bytestrings come in two flavors: strict and lazy ones. Strict bytestrings reside in Data.ByteString and they do away with the laziness completely.

There are no promises involved; a strict bytestring represents a series of bytes in an array. You can't have things like infinite strict bytestrings.

If you evaluate the first byte of a strict bytestring, you have to evaluate it whole. The upside is that there's less overhead because there are no thunks (the technical term for promise) involved.

The downside is that they're likely to fill your memory up faster because they're read into memory at once.

The other variety of bytestrings resides in Data.ByteString.Lazy. They're lazy, but not quite as lazy as lists. Like we said before, there are as many thunks in a list as there are elements.

That's what makes them kind of slow for some purposes. Lazy bytestrings take a different approach — they are stored in chunks (not to be confused with thunks!), each chunk has a size of 64K.

So if you evaluate a byte in a lazy bytestring (by printing it or something), the first 64K will be evaluated. After that, it's just a promise for the rest of the chunks.

Lazy bytestrings are kind of like lists of strict bytestrings with a size of 64K. When you process a file with lazy bytestrings, it will be read chunk by chunk. This is cool because it won't cause the memory usage to skyrocket and the 64K probably fits neatly into your CPU's L2 cache.

--}

{--

The function pack has the type signature pack :: [Word8] -> ByteString. What that means is that it takes a list of bytes of type Word8 and returns a ByteString. You can think of it as taking a list, which is lazy, and making it less lazy, so that it's lazy only at 64K intervals.

What's the deal with that Word8 type? Well, it's like Int, only that it has a much smaller range, namely 0-255. It represents an 8-bit number.

And just like Int, it's in the Num typeclass. For instance, we know that the value 5 is polymorphic in that it can act like any numeral type. Well, it can also take the type of Word8.

B.pack [98..120]
Chunk "bcdefghijklmnopqrstuvwx" Empty

fromChunks takes a list of strict bytestrings and converts it to a lazy bytestring. toChunks takes a lazy bytestring and converts it to a list of strict ones.

--}

res1 = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
-- Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))

{--

The bytestring version of : is called cons It takes a byte and a bytestring and puts the byte at the beginning. It's lazy though, so it will make a new chunk even if the first chunk in the bytestring isn't full.

That's why it's better to use the strict version of cons, cons' if you're going to be inserting a lot of bytes at the beginning of a bytestring.

--}

res2 = foldr B.cons B.empty [50..60]
-- Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<" Empty))))))))))

res3 = foldr B.cons' B.empty [50..60]
-- Chunk "23456789:;<" Empty

{-- Exceptions

Despite having expressive types that support failed computations, Haskell still has support for exceptions, because they make more sense in I/O contexts. A lot of things can go wrong when dealing with the outside world because it is so unreliable.

Okay, so I/O code (i.e. impure code) can throw exceptions. It makes sense. But what about pure code? Well, it can throw exceptions too.

Think about the div and head functions. They have types of (Integral a) => a -> a -> a and [a] -> a, respectively.

No Maybe or Either in their return type and yet they can both fail! div explodes in your face if you try to divide by zero and head throws a tantrum when you give it an empty list.

--}

{--

Pure code can throw exceptions, but it they can only be caught in the I/O part of our code (when we're inside a do block that goes into main). That's because you don't know when (or if) anything will be evaluated in pure code, because it is lazy and doesn't have a well-defined order of execution, whereas I/O code does.

However, once pure functions start throwing exceptions, it matters when they are evaluated. That's why we can only catch exceptions thrown from pure functions in the I/O part of our code.

And that's bad, because we want to keep the I/O part as small as possible. However, if we don't catch them in the I/O part of our code, our program crashes. The solution? Don't mix exceptions and pure code.

Take advantage of Haskell's powerful type system and use types like Either and Maybe to represent results that may have failed.

--}

{--

That's why we'll just be looking at how to use I/O exceptions for now. I/O exceptions are exceptions that are caused when something goes wrong while we are communicating with the outside world in an I/O action that's part of main.

--}

main0 = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

{--

Aha, we get an error from GHC, telling us that the file does not exist. Our program crashes. What if we wanted to print out a nicer message if the file doesn't exist?

One way to do that is to check if the file exists before trying to open it by using the doesFileExist function from System.Directory.

--}

main1 = do (fileName:_) <- getArgs
           fileExists <- doesFileExist fileName
           if fileExists
             then do contents <- readFile fileName
                     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
             else do putStrLn "The file doesn't exist!"

{--

To deal with this by using exceptions, we're going to take advantage of the catch function from System.IO.Error.

Its type is catch :: IO a -> (IOError -> IO a) -> IO a.

--}

main2 = toTry `catch` handler

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

{--

In the handler, we didn't check to see what kind of IOError we got. We just say "Whoops, had some trouble!" for any kind of error.

--}

main3 = toTry `catch` handler3

handler3 :: IOError -> IO ()
handler3 e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

{--

The predicates that act on IOError are:

isAlreadyExistsError
isDoesNotExistError
isAlreadyInUseError
isFullError
isEOFError
isIllegalOperation
isPermissionError
isUserError

isUserError evaluates to True when we use the function userError to make the exception, which is used for making exceptions from our code and equipping them with a string.

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | isFullError e = freeSomeSpace
    | isIllegalOperation e = notifyCops
    | otherwise = ioError e

Where notifyCops and freeSomeSpace are some I/O actions that you define.

Be sure to re-throw exceptions if they don't match any of your criteria, otherwise you're causing your program to fail silently in some cases where it shouldn't.

System.IO.Error also exports functions that enable us to ask our exceptions for some attributes, like what the handle of the file that caused the error is, or what the filename is.

These start with ioe and you can see a full list of them in the documentation.

we can use the ioeGetFileName function, which has a type of ioeGetFileName :: IOError -> Maybe FilePath. It takes an IOError as a parameter and maybe returns a FilePath (which is just a type synonym for String, remember, so it's kind of the same thing).

--}

main4 = toTry `catch` handler4

handler4 :: IOError -> IO ()
handler4 e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e

{--

You don't have to use one handler to catch exceptions in your whole I/O part. You can just cover certain parts of your I/O code with catch or you can cover several of them with catch and use different handlers for them, like so:

--}

{--

main4 = do toTry `catch` handler1
          thenTryThis `catch` handler2
          launchRockets

launchRockets isn't a parameter to catch, so whichever exceptions it might throw will likely crash our program, unless launchRockets uses catch internally to handle its own exceptions.

Throwing exceptions from pure code and dealing with them hasn't been covered here, mainly because, like we said, Haskell offers much better ways to indicate errors than reverting to I/O to catch them.

Even when glueing together I/O actions that might fail, I prefer to have their type be something like IO (Either a b), meaning that they're normal I/O actions but the result that they yield when performed is of type Either a b, meaning it's either Left a or Right b.

--}
