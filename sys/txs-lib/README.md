
## Testing the library from the GHCi repl

Load the txs-library

```sh
stack ghci txs-lib:lib
```

```haskell
cs <- readFile "examps/Echo/Echo.txs"
s <- newSession 
r <- load s cs
stepper s "Model"
getNextMsg s
step s (NumberOfSteps 1)
getNextMsg s
```
### Limitations

When testing this at the REPL, hitting Ctrl+C will kill the different processes
spawn by `TorXakis`, which might cause errors like:

```text
*** Exception: fd:14: hPutChar: resource vanished (Broken pipe)
```

because the `TorXakis` core cannot communicate with the SMT solver process for
instance.

So bear this in mind when testing.
