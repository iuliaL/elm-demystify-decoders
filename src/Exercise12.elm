module Exercise12 exposing (Tree(..), decoder)

import Json.Decode exposing (Decoder, lazy, field, int, map2, string, list)
import Json.Decode exposing (oneOf)



{- There's one more interesting use case we've completely skipped so far.
   Handling recursive data. So let's set the record straight.

   Recursive decoders look almost exactly like any other decoder, except for
   one thing, and that one - very essential - thing stems from the fact that
   Elm is an eager language: every expression is executed as soon as all
   parameters are available. When defining a recursive decoder, we're defining
   functions that don't explicitly take input; so their parameters are
   available, and you get into a self-refential loop.

   Now, of course, the Elm compiler has your back, and will dutifully tell you
   that you cannot do that, and even point out how to fix it. Thanks, Elm!

   Example input:

        var input = { "name": "parent", "children": [
            { "name": "foo", "value": 5 }, -- LEAF
            { "name": "empty", "children": [] } -- EMPTY BRANCH
        ]};

    Example output:

        Branch "parent"
            [ Leaf "foo" 5
            , Branch "empty" []
            ]
-}


type Tree
    = Branch String (List Tree)
    | Leaf String Int


decoder : Decoder Tree
decoder =
    oneOf [leafDecoder, branchDecoder]


leafDecoder : Decoder Tree
leafDecoder =
    map2 Leaf (field "name" string) (field "value" int)

branchDecoder: Decoder Tree
branchDecoder = map2 Branch (field "name" string) (field "children" (lazy (\() -> list decoder)))

{- Once you think you're done, run the tests for this exercise from the root of
   the project:

   - If you have installed `elm-test` globally:
        `elm-test tests/Exercise12`

   - If you have installed locally using `npm`:
        `npm run elm-test tests/Exercise12`

   - If you have installed locally using `yarn`:
        `yarn elm-test tests/Exercise12`
-}
