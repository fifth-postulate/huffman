module HuffmanTest exposing (suite)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Huffman exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Huffman"
        [ describe "creation"
            [ test "With empty frequency list should succeed" <|
                \_ ->
                    case table [ ( 'A', 1 ), ( 'B', 2 ) ] of
                        Just _ ->
                            pass

                        Nothing ->
                            fail "should have created a table"
            ]
        ]
