This package provides access to the `core` and `front` functionalities of
`TorXakis`. It's main goal is to provide the functions needed for developing
`TorXakis` tooling such as (graphical) user interfaces, web-services, etc.

## Testing the library from the GHCi repl

The [`Examples.hs`](src/TorXakis/Lib/Examples.hs) file contains examples of the
usage this library, along with some utility functions to explore the `TorXakis`
functionality. To use the `txs-lib` library functions from a `GHCi` session run
`stack`:

```sh
stack ghci txs-lib:lib
```

Next we show how to step through the [`Echo`](test/data/Echo.txs) model from
the `GHCi` repl.

Once in the `GHCi` prompt, read the model from a file:
```haskell
cs <- readFile "examps/Echo/Echo.txs"
-- Create a new session, and use it to load the model:
s <- newSession 
r <- load s cs
-- Initialize the stepper:
stepper s "Model"
-- Check that everything was correctly initialized by printing some messages:
printNextNMsgs s 3
```
Now we're ready to step!
```haskell
step s (NumberOfSteps 3)
printNextNMsgs s 3
```

Note that we can access to all the information of the actions that are returned
by the stepper.

And we can continue stepping from where we left off:
```haskell
step s (NumberOfSteps 2)
printNextNMsgs s 2
```

The step function is asynchronous, which means that it will return immediately.
This give the tool developers more flexibility. To see the verdict we need to
wait for it.

```haskell
r <- load s cs
stepper s "Model"
step s (NumberOfSteps 1000)
putStrLn "I can do somehting in between"
putStrLn "Get more messages"
printNextNMsgs s 10
verdict <- waitForVerdict s
putStrLn $ "The result was " ++ show verdict
```

### Limitations

When testing this at the REPL, hitting `Ctrl+C` will kill the different processes
spawn by `TorXakis`, which might cause errors like:
```text
*** Exception: fd:14: hPutChar: resource vanished (Broken pipe)
```
caused by the fact that the `TorXakis` core cannot communicate with the SMT
solver process (which was killed by the `Ctrl+C` signal). So bear this in mind
when playing the the `txs-lib` at the REPL.
