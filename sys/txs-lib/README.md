
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
