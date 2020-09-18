# pump (the brakes)

Given a haskell package, construct a build matrix of all its (valid) reverse dependencies. This is useful if you want to estimate downstream breakage introduced by an API change, e.g. by building all of the relevant packages against the patched haskell package.

A lot of the code for handling the hackage index/computing reverse dependencies therefrom was lifted from https://github.com/snoyberg/packdeps.

## Example

Building the 20 most popular reverse dependencies against it in a build matrix.

### Download the package index
```bash
$ pump download -o index
```

### Print out the 20 most popular dependencies of array (just for reference)
```bash
$ pump top -i index -p array -n 20
containers
text
deepseq
lens
binary
stm
attoparsec
http-types
Cabal
http-client
cereal
warp
quickcheck-instances
ghc
HTTP
haskell98
parallel
haskell-src-exts
MissingH
gtk
```

### Generate the build matrix
```bash
$ pump matrix -i index -p array -o matrix.json -n 20
```

### Realise the build matrix and generate a report
```
$ pump realise -m matrix.json -o report.json
```
