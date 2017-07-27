{-- Files and streams

getContents is an I/O action that reads everything from the standard input until it encounters an end-of-file character.

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

In terminal:

$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...

$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless

$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file

As you can see, piping the output of one program (in our case that was cat) to the input of another (capslocker) is done with the | character.

we can use getContents to make our program even shorter and better:

--}

import Data.Char
import System.IO

main1 = do
    contents <- getContents
    putStr (map toUpper contents)

{--

Keep in mind that because strings are basically lists, which are lazy, and getContents is I/O lazy, it won't try to read the whole content at once and store it into memory before printing out the capslocked version.

Rather, it will print out the capslocked version as it reads it, because it will only read a line from the input when it really needs to.

$ ./capslocker
hey ho
HEY HO
lets go
LETS GO

and this repeats until there's no more input, which is signified by an end-of-file character. If we not hit CTRL-D as EOL in STDIN yet, then getContents will not stopped. The code is actually as similar scala code:

getContents.flatMap(contents => (map toUpper contents))

And getContents will check buffer as getChar did, if the buffer not meet EOL, go read new input.

--}

{--

interact takes a function of type String -> String as a parameter and returns an I/O action that will take some input, run that function on it and then print out the function's result.

main2 == main3

--}

main2 = do
    contents <- getContents
    putStr (shortLinesOnly contents)


main3 = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

{--

we can do this even shorter:

--}

main4 = interact $ unlines . filter ((<10) . length) . lines

respondPalindromes contents = unlines (map (\xs ->
  if isPalindrome xs
    then "palindrome"
    else "not a palindrome"
  ) (lines contents))
    where   isPalindrome xs = xs == reverse xs

-- point free style

respondPalindromes2 = unlines . map (\xs ->
  if isPalindrome xs
    then "palindrome"
    else "not a palindrome"
  ) . lines
  where isPalindrome xs = xs == reverse xs

main5 = interact respondPalindromes
main6 = interact respondPalindromes2


main7 = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

{-- File IO

openFile :: FilePath -> IOMode -> IO Handle

FilePath is just a type synonym for String

IOMode is a type that's defined like this:

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

Finally, it returns an I/O action that will open the specified file in the specified mode. If we bind that action to something we get a Handle. A value of type Handle represents where our file is.

--}
