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
