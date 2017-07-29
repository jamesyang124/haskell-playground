module Chp93
where

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
import System.Directory
import Data.List


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

we see a function called hGetContents. It takes a Handle, so it knows which file to get the contents from and returns an IO String — an I/O action that holds as its result the contents of the file.

This function is pretty much like getContents. The only difference is that getContents will automatically read from the standard input (that is from the terminal), whereas hGetContents takes a file handle which tells it which file to read from.

And just like getContents, hGetContents won't attempt to read the file at once and store it in memory, but it will read it as needed. That's really cool because we can treat contents as the whole contents of the file, but it's not really loaded in memory.

So if this were a really huge file, doing hGetContents wouldn't choke up our memory, but it would read only what it needed to from the file, when it needed to.

If you imagine your whole file system to be a really big book and each file is a chapter in the book, the handle is a bookmark that shows where you're currently reading (or writing) a chapter, whereas the contents are the actual chapter.

With putStr contents we just print the contents out to the standard output and then we do hClose, which takes a handle and returns an I/O action that closes the file. You have to close the file yourself after opening it with openFile

Another way of doing what we just did is to use the withFile function, which has a type signature of -

withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

--}

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

{--

Just like we have hGetContents that works like getContents but for a specific file, there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc.

They work just like their counterparts without the h, only they take a handle as a parameter and operate on that specific file instead of operating on standard input or standard output.

--}

{--

It's usually more handy than doing openFile and binding it to a handle and then doing hGetContents. Here's how we could have written our previous example with readFile:

--}

main8 = do
    contents <- readFile "girlfriend.txt"
    putStr contents

{--

writeFile has a type of writeFile :: FilePath -> String -> IO ()

If such a file already exists, it will be stomped down to zero length before being written on.

--}

main9 = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)

main10 = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

{--

We needed to add the "\n" to the end of each line because getLine doesn't give us a newline character at the end.

We talked about how doing contents <- hGetContents handle doesn't cause the whole file to be read at once and stored in-memory. It's I/O lazy, so doing this:

--}

main11 = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

{--

It is actually like connecting a pipe from the file to the output. Just like you can think of lists as streams, you can also think of files as streams. This will read one line at a time and print it out to the terminal as it goes along.

Well, for text files, the default buffering is line-buffering usually. That means that the smallest part of the file to be read at once is one line.

For binary files, the default buffering is usually block-buffering. That means that it will read the file chunk by chunk. The chunk size is some size that your operating system thinks is cool.

You can control how exactly buffering is done by using the hSetBuffering function. It takes a handle and a BufferMode and returns an I/O action that sets the buffering.

BufferMode is a simple enumeration data type and the possible values it can hold are:

1. NoBuffering
2. LineBuffering
3. BlockBuffering (Maybe Int).

The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing, then the operating system determines the chunk size.

NoBuffering means that it will be read one character at a time. NoBuffering usually sucks as a buffering mode because it has to access the disk so much.

--}

{--

We can also use hFlush, which is a function that takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle.

When we're doing line-buffering, the buffer is flushed after every line. When we're doing block-buffering, it's after we've read a chunk. It's also flushed after closing a handle.

But we can use hFlush to force that reporting of data that has been read so far. After flushing, the data is available to other programs that are running at the same time.

--}

{--

Next up, we use a function that we haven't met before which is from System.IO — openTempFile. Its name is pretty self-explanatory.

It takes a path to a temporary directory and a template name for a file and opens a temporary file. We used "." for the temporary directory, because . denotes the current directory on just about any OS.

--}

main12 = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
