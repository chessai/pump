# pump (the brakes)

Given a haskell package, construct a build matrix of all its (valid) reverse dependencies. This is useful if you want to estimate downstream breakage introduced by an API change, e.g. by building all of the relevant packages against the patched haskell package.

A lot of this code was lifted from https://github.com/snoyberg/packdeps.
