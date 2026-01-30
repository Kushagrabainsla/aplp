# Haskell Assignment 1


## Run a single Haskell file

### Using bash script
```
./run_haskell.sh assignment1/part1.hs
```

### Or manually
```
ghc -o myprog part1.hs
./myprog
```

## Run the JSON example (with dependencies)

```
ghc -o jsonDriver jsonDriver.hs JSON.hs
./jsonDriver
```
