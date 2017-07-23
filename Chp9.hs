main = putStrLn "hello, world"

{--

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
