Implementations of common data structures in Haskell.


# Tests

To start ghci with a specific test configuration as context: 
```
cabal repl test-datastructures
```


To run the arbitrary instance using Ints as the tree contents, from within
ghci: 
```
ghci> sample (arbitrary ::Gen (RBTree Int))
```

To run the tests from the command line: 
```
% cabal test test-datastructures
```
