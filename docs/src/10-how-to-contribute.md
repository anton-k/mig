# How to contribute

If you are interested in contribution to library that's great.
We have a list of upcoming features in the [list of Issues](https://github.com/anton-k/mig/issues).

Also there is a CI which checks for:

* build and test of all libraries with stack
* build and test of all examples in the directory `examples` with stack
* `fourmolu` formatting

## how to automate formatter check

To check for formatting I recommend to use this pre-commit hook:

```bash
#!/bin/bash
 
if command -v fourmolu &> /dev/null
then
  files=$(git diff --staged --name-only -- '*.hs')
  for file in $files
  do
    fourmolu -i $file
    git add $file
  done
else
  echo "fourmolu cannot be found"
  echo "install fourmolu via cabal install"
fi

if command -v cabal-fmt &> /dev/null
then
  files=$(git diff --staged --diff-filter=ACMR --name-only -- '*.cabal')
  for file in $files
  do
    cabal-fmt --inplace --no-tabular $file
    git add $file
  done
else
  echo "cabal-fmt cannot be found"
fi
```

Save this as file `.git/hooks/pre-commit` in your repo
and make it executable:

```sh
chmod +x .git/hooks/pre-commit
```

The script requires two executables which both can be installed from Hackage:

* `fourmolu`
* `cabal-fmt`

After that all files that you modify will be formatted properly.
Formatting settings are in the file `fourmolu.yaml`.
