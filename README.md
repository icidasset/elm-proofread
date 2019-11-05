__Elm Proofread.__  
_This command-line tool runs your Elm documentation tests._

```elm
module Example exposing (add)

{-| Add two integers together.

    >>> add 1 2
    3

    >>> import Tuple
    >>> nine =
    ..>   Tuple.first ( 9, True )

    >>> 1.0
    ..>     |> round
    ..>     |> add nine
    add
        (round 5.0)
        5

-}
add : Int -> Int -> Int
add x y = x + y
```


### How to use

```shell
# Proofread a single file
elm-proofread src/Main.elm

# Proofread stdin
cat src/Main.elm | elm-proofread

# Go to a directory, find all the Elm files and proofread all of them until one fails
( cd elm-project && \
  find . -name "*.elm" -print0 | \
  xargs -0 -n 1 -I % sh -c 'elm-proofread -- % || exit 255; echo "\n\n"' \
)
```

Built for `Elm v0.19`.  
This command assumes the Elm REPL can be run and can locate an `elm.json` file.


### How to install

- Option 1, use one of the prebuilt binaries available on the [releases page](releases).
- Option 2, use [Haskell Stack](https://www.haskellstack.org/).

  ```shell
  git clone git@github.com:icidasset/elm-proofread.git
  cd elm-proofread
  stack install
  ```
