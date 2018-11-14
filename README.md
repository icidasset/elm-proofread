__Elm Proofread.__  
_This command-line tool runs your Elm documentation tests._

Built for `Elm v0.19`.  
This command assumes the Elm REPL can be run and can locate an `elm.json` file.


```shell
# Proofread a single file
elm-proofread src/Main.elm

# Proofread stdin
cat src/Main.elm | elm-proofread

# Go to a directory, find all the Elm files and proofread all of them until one fails
( cd elm-project && \
  find . -name "*.elm" -print0 | \
  xargs -0 -n 1 sh -c 'elm-proofread -- $0 || exit 255; echo "\n\n"'
)
```

_Note: The Elm REPL can locate an `elm.json` file if the current working-directory is a sub directory of the Elm project._
