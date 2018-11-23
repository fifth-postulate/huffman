module Huffman exposing (Tree(..), build, leaf, merge, occurence, table)

{-| Encode and decode data via a [Huffman code](https://en.wikipedia.org/wiki/Huffman_coding).
-}

import PriorityQueue exposing (Priority, PriorityQueue)


{-| Huffman code tree.

Symbols are encoded by the path from the root to the leaf that contains the symbol. Taking the left sub-tree encodes a `0`. Taking the right tree sub-tree encodes a `1`. It produces a [_prefix code_](https://en.wikipedia.org/wiki/Prefix_code).

> a type of code system (typically a variable-length code) distinguished by its possession of the "prefix property", which requires that there is no whole code word in the system that is a prefix (initial segment) of any other code word in the system.

-}
type Tree symbol
    = Leaf Int symbol
    | Node Int (Tree symbol) (Tree symbol)


{-| Create a Huffman code tree from a list of occurences of symbols.

Table will fail to construct a Huffman code tree when there are no symbols to encode.

-}
table : List ( symbol, Int ) -> Maybe (Tree symbol)
table frequencyList =
    let
        priority tree =
            occurence tree

        empty =
            PriorityQueue.empty priority

        queue =
            frequencyList
                |> List.map leaf
                |> List.foldr PriorityQueue.insert empty
    in
    build queue


{-| Create a Huffman code tree from a `PriorityQueue`.
-}
build : PriorityQueue (Tree symbol) -> Maybe (Tree symbol)
build queue =
    case PriorityQueue.take 2 queue of
        [ left, right ] ->
            let
                tree =
                    merge left right

                nextQueue =
                    queue
                        |> PriorityQueue.drop 2
                        |> PriorityQueue.insert tree
            in
            build nextQueue

        [ tree ] ->
            Just tree

        _ ->
            Nothing


{-| Create a leaf of a Huffman code tree.
-}
leaf : ( symbol, Int ) -> Tree symbol
leaf ( symbol, occurences ) =
    Leaf occurences symbol


{-| Merge tow Huffman code trees into one.
-}
merge : Tree symbol -> Tree symbol -> Tree symbol
merge left right =
    let
        combinedOccurence =
            occurence left + occurence right
    in
    Node combinedOccurence left right


{-| Count the combined occurences of all the symbols in the tree.
-}
occurence : Tree symbol -> Int
occurence tree =
    case tree of
        Leaf c _ ->
            c

        Node c _ _ ->
            c
