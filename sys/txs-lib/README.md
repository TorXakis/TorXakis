
## Testing the library from the GHCi repl

Load the txs-library

```sh
stack ghci txs-lib:lib
```

```haskell
>>> cs <- readFile "path/to/your/torxakis/Model.txs"
>>> s <- newSession 
>>> r <- load s cs
```
