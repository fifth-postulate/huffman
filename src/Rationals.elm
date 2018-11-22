module Rationals exposing (Fraction, add, compare, divide, fraction, multiply, one, subtract)

{-| The rational numbers.

A [_rational number_](https://en.wikipedia.org/wiki/Rational_number) is

> any number that can be expressed as the quotient or fraction `p/q` of two integers, a numerator p and a non-zero denominator q.


# Usage

The following code creates and then adds two fractions.

    p = fraction 1 3
    q = fraction 1 4

    (add p q) == fraction 7 12 -- True

-}


{-| Represents a _rational number_.
-}
type Fraction
    = Fraction { numerator : Int, denominator : Int }


{-| The constant 1 as a `Fraction`.
-}
one : Fraction
one =
    fraction 1 1


{-| Create a `Fraction` with a given numerator and denominator.

If you would want to make the fraction `37/51` one uses

    fraction 37 51

-}
fraction : Int -> Int -> Fraction
fraction numerator denominator =
    let
        s =
            sign numerator * sign denominator

        absoluteNumerator =
            abs numerator

        absoluteDenominator =
            abs denominator

        d =
            gcd absoluteNumerator absoluteDenominator

        reducedNumerator =
            s * absoluteNumerator // d

        reducedDenominator =
            absoluteDenominator // d
    in
    Fraction { numerator = reducedNumerator, denominator = reducedDenominator }


{-| determines the [greatest common divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of two integers.
-}
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


{-| Determines the sign of a `Int`.

    sign -37 == -1

    sign 0 == 0

    sign 51 == 1

-}
sign : Int -> Int
sign n =
    case Basics.compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


{-| Adds two `Fraction`s.
-}
add : Fraction -> Fraction -> Fraction
add (Fraction p) (Fraction q) =
    let
        numerator =
            q.denominator * p.numerator + p.denominator * q.numerator

        denominator =
            p.denominator * q.denominator
    in
    fraction numerator denominator


{-| Subtracts two `Fraction`s.
-}
subtract : Fraction -> Fraction -> Fraction
subtract (Fraction p) (Fraction q) =
    let
        numerator =
            q.denominator * p.numerator - p.denominator * q.numerator

        denominator =
            p.denominator * q.denominator
    in
    fraction numerator denominator


{-| Multiply two `Fraction`s.
-}
multiply : Fraction -> Fraction -> Fraction
multiply (Fraction p) (Fraction q) =
    let
        numerator =
            p.numerator * q.numerator

        denominator =
            p.denominator * q.denominator
    in
    fraction numerator denominator


{-| Divide two `Fraction`s.
-}
divide : Fraction -> Fraction -> Fraction
divide (Fraction p) (Fraction q) =
    let
        numerator =
            p.numerator * q.denominator

        denominator =
            p.denominator * q.numerator
    in
    fraction numerator denominator


{-| Compares two `Fraction`s and returns their `Order`.
-}
compare : Fraction -> Fraction -> Order
compare (Fraction p) (Fraction q) =
    let
        pn =
            p.numerator * q.denominator

        qn =
            q.numerator * p.denominator
    in
    Basics.compare pn qn
