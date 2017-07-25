import Data.Char

{--

main = putStrLn "hello, world"

:t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()

putStrLn takes a string and returns an I/O action that has a result type of () (i.e. the empty tuple, also know as unit).

An I/O action is something that, when performed, will carry out an action with a side-effect (that's usually either reading from the input or printing stuff to the screen) and will also contain some kind of return value inside it.

Printing a string to the terminal doesn't really have any kind of meaningful return value, so a dummy value of () is used.

The empty tuple is a value of () and it also has a type of ()

--}

{--

So, when will an I/O action be performed? Well, this is where main comes in. An I/O action will be performed when we give it a name of main and then run our program.

--}

main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

{--

Notice that we said do and then we laid out a series of steps, like we would in an imperative program. Each of these steps is an I/O action. By putting them together with do syntax, we glued them into one I/O action.

main always has a type signature of main :: IO something, where something is some concrete type. By convention, we don't usually specify a type declaration for main.

ghci> :t getLine
getLine :: IO String

Notice that name <- getLine syntax

Once it's fetched that data for you, the only way to open the box and get the data inside it is to use the <- construct. And if we're taking data out of an I/O action, we can only take it out when we're inside another I/O action. This is how Haskell manages to neatly separate the pure and impure parts of our code.

"getLine" is in a sense impure because its result value is not guaranteed to be the same when performed twice.

That's why it's sort of tainted with the IO type constructor and we can only get that data out in I/O code.

And because I/O code is tainted too, any computation that depends on tainted I/O data will have a tainted result.

--}

{--

When I say tainted, I don't mean tainted in such a way that we can never use the result contained in an I/O action ever again in pure code. No, we temporarily un-taint the data inside an I/O action when we bind it to a name.

Every I/O action that gets performed has a result encapsulated within it. That's why our previous example program could also have been written like this:

--}

main2 = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

{--

However, foo would just have a value of (), so doing that would be kind of moot.

In a do block, the last action cannot be bound to a name like the first two were. We'll see exactly why that is so a bit later when we venture off into the world of monads.

Beginners sometimes think that doing

name = getLine

will read from the input and then bind the value of that to name. Well, it won't, all this does is give the getLine I/O action a different name called, well, name.

Remember, to get the value out of an I/O action, you have to perform it inside another I/O action by binding it to a name with <-.

I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O action that we composed with a do block.

We can also use a do block to glue together a few I/O actions and then we can use that I/O action in another do block and so on. Either way, they'll be performed only if they eventually fall into main.

--}

main3 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

{--

You may be wondering when to use <- and when to use let bindings?

Well, remember, <- is (for now) for performing I/O actions and binding their results to names.

map toUpper firstName, however, isn't an I/O action. It's a pure expression in Haskell.

--}

main5 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{--

It's just a normal function that takes a string like "hey there man" and then calls words with it to produce a list of words like ["hey","there","man"]. Then we map reverse on the list, and then we put that back into one string by using unwords and the final result is "yeh ereht nam".

Remember that in Haskell, every if must have a corresponding else because every expression has to have some sort of value.

We make the if so that when a condition is true (in our case, the line that we entered is blank), we perform one I/O action and when it isn't, the I/O action under the else is performed.

That's why in an I/O do block, ifs have to have a form of "if condition then I/O action else I/O action".

Because, we have to have exactly one I/O action after the else, we use a do block to glue together two I/O actions into one. You could also write that part out as:

else (do
    putStrLn $ reverseWords line
    main)

Well, here's the thing: the return in Haskell is really nothing like the return in most other languages!

In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value.

you think about the box analogy from before, it takes a value and wraps it up in a box.

The resulting I/O action doesn't actually do anything, it just has that value encapsulated as its result. So in an I/O context, return "haha" will have a type of IO String.

Well, we needed some I/O action to carry out in the case of an empty input line. That's why we just made a bogus I/O action that doesn't do anything by writing return ().

--}

{--

Using return doesn't cause the I/O do block to end in execution or anything like that.

this program will quite happily carry out all the way to the last line:

--}

main6 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

{--

All these returns do is that they make I/O actions that don't really do anything except have an encapsulated result and that result is thrown away because it isn't bound to a name.

--}

main7 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

{--

So you see, return is sort of the opposite to <-. While return takes a value and wraps it up in a box, <- takes a box (and performs it) and takes the value out of it, binding it to a name.

Could just use let bindings.

we mostly use return either because we need to create an I/O action that doesn't do anything or because we don't want the I/O action that's made up from a do block to have the result value of its last action, but we want it to have a different result value, so we use return to make an I/O action that always has our desired result contained and we put it at the end.

--}

main8 = do  putStr "Hey, "
            putStr "I'm "
            putStrLn "Andy!"

{--

putStr is much like putStrLn in that it takes a string as a parameter and returns an I/O action that will print that string to the terminal, only putStr doesn't jump into a new line after printing out the string while putStrLn does.

--}

main9 = do  putChar 't'
            putChar 'e'
            putChar 'h'

{--

putChar takes a character and returns an I/O action that will print it out to the terminal.

--}

main10 = do print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]

{--

print takes a value of any type that's an instance of Show (meaning that we know how to represent it as a string), calls show with that value to stringify it and then outputs that string to the terminal. Basically, it's just putStrLn . show.

--}

main11 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

{--

Thus, its type signature is getChar :: IO Char, because the result contained within the I/O action is a Char. Note that due to buffering, reading of the characters won't actually happen until the user mashes the return key.

$ runhaskell getchar_test.hs
hello sir
hello

The second line is the input. We input hello sir and then press return. Due to buffering, the execution of the program will begin only when after we've hit return and not after every inputted character. But once we press return, it acts on what we've been putting in so far.

getChar / getLine will first open a file descriptor of STDIN input as line mode buffering(STDIN default mode). Once buffer flushed(by new line), this buffered input will consumed by getChar and putChar once at a char in each time.

--}

{--

https://stackoverflow.com/questions/13004801/why-is-executing-getchar-in-a-terminal-different-to-executing-it-in-ghci

getChar is the same as hGetChar stdin, source code:

hGetChar handle =
  ...

  buf0 <- readIORef haCharBuffer

  -- try get buf0, if it is empty, read from file device, ex: STDIN
  buf1 <- if isEmptyBuffer buf0
           then readTextDevice handle_ buf0
           else return buf0

  (c1,i) <- readCharBuf (bufRaw buf1) (bufL buf1)
  let buf2 = bufferAdjustL i buf1
  -- get consumed left most char buffer

  if haInputNL == CRLF && c1 == '\r'
    ...
  else do
            writeIORef haCharBuffer buf2
            -- write buf2 back to haCharBuffer
            return c1
            -- return first char

--}
