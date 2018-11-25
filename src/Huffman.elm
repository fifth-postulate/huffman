module Huffman exposing (Bit(..), Tree(..), build, decode, encodeString, frequencies, leaf, merge, occurence, tree)

{-| Encode and decode data via a [Huffman code](https://en.wikipedia.org/wiki/Huffman_coding).
-}

import Dict exposing (Dict)
import PriorityQueue exposing (Priority, PriorityQueue)


{-| Decode, with a tree, an encoding into a list of symbols.
-}
decode : Tree symbol -> Encoding -> Maybe (List symbol)
decode aTree encoding =
    tailCallDecode [] aTree encoding
        |> Maybe.map List.reverse


{-| Tail call variant of decode.
-}
tailCallDecode : List symbol -> Tree symbol -> Encoding -> Maybe (List symbol)
tailCallDecode accumulator aTree encoding =
    case encoding of
        [] ->
            Just accumulator

        _ ->
            -- We are not using Maybe.map here because that wouldn't make it a tail call.
            case lookup aTree encoding of
                Just ( symbol, rest ) ->
                    tailCallDecode (symbol :: accumulator) aTree rest

                Nothing ->
                    Nothing


{-| Lookup the next symbol in the encoding
-}
lookup : Tree symbol -> Encoding -> Maybe ( symbol, Encoding )
lookup aTree encoding =
    case aTree of
        Leaf _ symbol ->
            Just ( symbol, encoding )

        Node _ left right ->
            case encoding of
                bit :: rest ->
                    case bit of
                        O ->
                            lookup left rest

                        I ->
                            lookup right rest

                [] ->
                    Nothing


{-| Encode a string.
-}
encodeString : String -> Maybe ( Tree Char, Encoding )
encodeString word =
    word
        |> String.toList
        |> encode


{-| Encode a list of symbols.
-}
encode : List comparable -> Maybe ( Tree comparable, Encoding )
encode list =
    let
        aTree =
            list
                |> frequencies
                |> tree

        table =
            aTree
                |> Maybe.map fromTree
                |> Maybe.withDefault (Table Dict.empty)

        encoder symbol =
            toEncoding symbol table

        concat x y =
            case ( x, y ) of
                ( Just xs, Just ys ) ->
                    Just (xs ++ ys)

                _ ->
                    Nothing

        cat x y =
            case ( x, y ) of
                ( Just u, Just v ) ->
                    Just ( u, v )

                _ ->
                    Nothing
    in
    list
        |> List.map encoder
        |> List.foldr concat (Just [])
        |> cat aTree


{-| Encodes a single symbol.
-}
toEncoding : comparable -> Table comparable -> Maybe Encoding
toEncoding symbol (Table dictionary) =
    Dict.get symbol dictionary


{-| Create a Hufmman table from a Huffman code tree.
-}
fromTree : Tree comparable -> Table comparable
fromTree aTree =
    aTree
        |> encodings
        |> Dict.fromList
        |> Table


{-| Return all encodings present in the `Tree`.
-}
encodings : Tree symbol -> List ( symbol, Encoding )
encodings aTree =
    let
        reverseEncoding ( symbol, encoding ) =
            ( symbol, List.reverse encoding )
    in
    aTree
        |> encodingsWithPrefix []
        |> List.map reverseEncoding


{-| Tail call variant of encodings.
-}
encodingsWithPrefix : Encoding -> Tree symbol -> List ( symbol, Encoding )
encodingsWithPrefix prefix aTree =
    case aTree of
        Leaf _ symbol ->
            [ ( symbol, prefix ) ]

        Node _ left right ->
            let
                subEncodings bit subtree =
                    encodingsWithPrefix (bit :: prefix) subtree
            in
            subEncodings O left ++ subEncodings I right


{-| A Huffman table

This can be used to encode symbols into `Encoding`s

    toEncoding table 'a'

-}
type Table comparable
    = Table (Dict comparable Encoding)


{-| Alias to represent an encoding of a symbol
-}
type alias Encoding =
    List Bit


{-| Represents a bit.

Is used in determining encoding for a symbol.

-}
type Bit
    = O
    | I


{-| Returns the frequencies of each element in the list.
-}
frequencies : List comparable -> List ( comparable, Int )
frequencies list =
    tailCallFrequencies Dict.empty list


{-| Tall call variant of frequencies.
-}
tailCallFrequencies : Dict comparable Int -> List comparable -> List ( comparable, Int )
tailCallFrequencies accumulator list =
    case list of
        [] ->
            Dict.toList accumulator

        head :: tail ->
            let
                increment : Maybe Int -> Maybe Int
                increment n =
                    n
                        |> Maybe.map (\i -> i + 1)
                        |> Maybe.withDefault 1
                        |> Just
            in
            tailCallFrequencies (Dict.update head increment accumulator) tail


{-| Huffman code tree.

Symbols are encoded by the path from the root to the leaf that contains the symbol. Taking the left sub-tree encodes a `0`. Taking the right tree sub-tree encodes a `1`. It produces a [_prefix code_](https://en.wikipedia.org/wiki/Prefix_code).

> a type of code system (typically a variable-length code) distinguished by its possession of the "prefix property", which requires that there is no whole code word in the system that is a prefix (initial segment) of any other code word in the system.

-}
type Tree symbol
    = Leaf Int symbol
    | Node Int (Tree symbol) (Tree symbol)


{-| Create a Huffman code tree from a list of occurences of symbols.

`tree` will fail to construct a Huffman code tree when there are no symbols to encode.

-}
tree : List ( symbol, Int ) -> Maybe (Tree symbol)
tree frequencyList =
    let
        priority aTree =
            occurence aTree

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
                parent =
                    merge left right

                nextQueue =
                    queue
                        |> PriorityQueue.drop 2
                        |> PriorityQueue.insert parent
            in
            build nextQueue

        [ aTree ] ->
            Just aTree

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
occurence aTree =
    case aTree of
        Leaf c _ ->
            c

        Node c _ _ ->
            c
