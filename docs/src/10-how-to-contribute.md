# How to contribute

If you are interested in contribution to library that's great.
We have a list of upcoming features in the [list of Issues](https://github.com/anton-k/mig/issues).

Also there is a CI which checks for:

* build and test of all libraries with stack
* build and test of all examples in the directory `examples` with stack
* `fourmolu` formatting
* deploy of docs/tutorial to github pages (on push to main branch)

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
```

Save this as file `.git/hooks/pre-commit` in your repo
and make it executable:

```sh
chmod +x .git/hooks/pre-commit
```

The script requires `fourmolu` executable which both can be installed from Hackage.

After that all files that you modify will be formatted properly.
Formatting settings are in the file `fourmolu.yaml`.
