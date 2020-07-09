# Build profunctors against version that uses QuantifiedConstraints

## 1. Build the package
```
cabal v2-build
```

## 2. Generate the package index
```
# dumps package index as binary to file `index`
pump download -o index
```

## 3. Generate the build matrix for `profunctors`
You will need the overrides.json provided:
```
pump matrix -i index -p profunctors -o matrix.json --prettify --overrides overrides.json
```

## 4. Run the builds and generate a report (will take a long time)
```
pump realise -m matrix.json -o report.json --prettify
```
