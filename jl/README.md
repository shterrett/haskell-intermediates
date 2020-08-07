# jl

This project will build a command-line application similar to [`jq`](https://stedolan.github.io/jq/) as part of an effort to provide intermediate level Haskell tutorials.

## Steps

### Pretty Printing

`jl` will take a json string on stdin and return a pretty-printed json string on
stdout.

Example output:

```json
{
  "thing": {
    "nestedThings": [
      "a thing",
      "another thing"
    ],
    "thingProperties": {
      "stuff": "things",
      "count": 3,
      "useful": false
    }
  }
}
```

First, create a `src/JL/PrettyPrint.hs` file as well as an accompanying
`test/JL/PrettyPrintSpec.hs` test file.

The first step will be to set up the testing library. This example is going to
use [`hspec`](http://hspec.github.io/). Add `hspec` and `hspec-discover` to the
`stack.yaml` and then add the auto-spec-discovery incantation in `test/Spec.hs`:
```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```
Ensure that each test file exports a top-level binding
```haskell
spec :: Spec
spec = <your tests>
```

Then, simply implement `prettyPrint` and test it.

### Focusing

`jl` will take an argument that represents a path into the json tree. It will
then follow that path and focus on that portion of the tree, passing only that
to the pretty printer.

If the given path does not return any results in the json value, an error should
be reported. As an extension, the longest path that does return results should
be printed, along with the results.

Syntax:
```
.<name> --> focus on the key named "name"
[*] --> continue on every element of the array; collect the elements in an array
[<number>] --> focus on the "number"-th element of an array; do not keep the
array in the results
```

The actual argument will look like
```
.firstKey.secondKey[*].thirdKey[1]
```
