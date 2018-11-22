module RationalsTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Rationals exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Rationals"
        [ describe "arithmetic"
            [ describe "addition"
                [ test "1/3 + 1/4" <|
                    \_ ->
                        let
                            p =
                                fraction 1 3

                            q =
                                fraction 1 4

                            actual =
                                add p q

                            expected =
                                fraction 7 12
                        in
                        Expect.equal actual expected
                , test "1/3 + 2/3" <|
                    \_ ->
                        let
                            p =
                                fraction 1 3

                            q =
                                fraction 2 3

                            actual =
                                add p q

                            expected =
                                one
                        in
                        Expect.equal actual expected
                ]
            , describe "subtraction"
                [ test "1/3 - 1/4" <|
                    \_ ->
                        let
                            p =
                                fraction 1 3

                            q =
                                fraction 1 4

                            actual =
                                subtract p q

                            expected =
                                fraction 1 12
                        in
                        Expect.equal actual expected
                , test "1/3 - 2/3" <|
                    \_ ->
                        let
                            p =
                                fraction 1 3

                            q =
                                fraction 2 3

                            actual =
                                subtract p q

                            expected =
                                fraction -1 3
                        in
                        Expect.equal actual expected
                ]
            , describe "multiplication"
                [ test "2/3 * 3/4" <|
                    \_ ->
                        let
                            p =
                                fraction 2 3

                            q =
                                fraction 3 4

                            actual =
                                multiply p q

                            expected =
                                fraction 1 2
                        in
                        Expect.equal actual expected
                ]
            , describe "division"
                [ test "2/3 / 3/4" <|
                    \_ ->
                        let
                            p =
                                fraction 2 3

                            q =
                                fraction 3 4

                            actual =
                                divide p q

                            expected =
                                fraction 8 9
                        in
                        Expect.equal actual expected
                ]
            ]
        , describe "comparison"
            [ test "compare 1/3 1/2" <|
                \_ ->
                    let
                        p =
                            fraction 2 3

                        q =
                            fraction 3 4

                        actual =
                            Rationals.compare p q

                        expected =
                            LT
                    in
                    Expect.equal actual expected
            ]
        ]
