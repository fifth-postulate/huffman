module HuffmanTest exposing (suite)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Huffman exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Huffman"
        [ describe "frequencies"
            [ test "of characters should be correctly determined" <|
                \_ ->
                    let
                        actual =
                            "ABBCCCDDE"
                                |> String.toList
                                |> frequencies

                        expected =
                            [ ( 'A', 1 ), ( 'B', 2 ), ( 'C', 3 ), ( 'D', 2 ), ( 'E', 1 ) ]
                    in
                    Expect.equalLists actual expected
            ]
        , describe "creation"
            [ test "With empty frequency list should succeed" <|
                \_ ->
                    case tree [ ( 'A', 1 ), ( 'B', 2 ) ] of
                        Just _ ->
                            pass

                        Nothing ->
                            fail "should have created a table"
            ]
        , describe "encodeString"
            [ test "\"CBCABC\" should succeed" <|
                \_ ->
                    let
                        result =
                            encodeString "CBCACBC"

                        expected =
                            [ I, O, I, I, O, O, I, O, I, I ]
                    in
                        case result of
                            Just (_, actual) ->
                                Expect.equal actual expected

                            Nothing ->
                                fail "should have encoded string"
            ]
        ]
