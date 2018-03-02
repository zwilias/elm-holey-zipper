module List.Holey.ZipperTest exposing (..)

import Expect
import List.Holey.Zipper as Zipper exposing (Full, Hole, Zipper)
import Test exposing (..)


empty : Test
empty =
    test "Empty creates a zipper for an empty list" <|
        \_ ->
            Zipper.empty
                |> Zipper.toList
                |> Expect.equal []


singleton : Test
singleton =
    test "Singleton creates a zipper with a single element" <|
        \_ ->
            Zipper.singleton 3
                |> Zipper.toList
                |> Expect.equal [ 3 ]


zipper : Test
zipper =
    let
        zipper : Zipper Full Int
        zipper =
            Zipper.zipper 1 [ 2, 3, 4, 5 ]
    in
    describe "zipper"
        [ test "Zipper creates a zipper." <|
            \_ ->
                zipper
                    |> Zipper.toList
                    |> Expect.equal [ 1, 2, 3, 4, 5 ]
        , test "nothing before it" <|
            \_ ->
                zipper
                    |> Zipper.before
                    |> Expect.equal []
        , test "the current thing is the first thing" <|
            \_ ->
                zipper
                    |> Zipper.current
                    |> Expect.equal 1
        , test "the current thing is followed by the rest of the things" <|
            \_ ->
                zipper
                    |> Zipper.after
                    |> Expect.equal [ 2, 3, 4, 5 ]
        ]



-- Navigation


next : Test
next =
    let
        zipper : Zipper Full Int
        zipper =
            Zipper.zipper 1 [ 2, 3 ]
    in
    describe "next"
        [ test "next gives the next thing" <|
            \_ ->
                Zipper.next zipper
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 2)
        , test "next on the next hole gives the next thing" <|
            \_ ->
                Zipper.nextHole zipper
                    |> Zipper.next
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 2)
        , test "next on last gives nothing" <|
            \_ ->
                Zipper.last zipper
                    |> Zipper.next
                    |> Expect.equal Nothing
        , test "repeating `next` eventually results in `Nothing`" <|
            \_ ->
                List.foldl Maybe.andThen (Just zipper) (List.repeat 4 Zipper.next)
                    |> Expect.equal Nothing
        ]


previous : Test
previous =
    let
        zipper : Zipper Full Int
        zipper =
            Zipper.zipper 1 [ 2, 3 ]
    in
    describe "previous"
        [ test "previous gives nothing initially" <|
            \_ ->
                Zipper.previous zipper
                    |> Expect.equal Nothing
        , test "previous on the last thing gives the thing before that" <|
            \_ ->
                Zipper.last zipper
                    |> Zipper.previous
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 2)
        , test "previous after the last hole gives the last thing" <|
            \_ ->
                Zipper.afterLast zipper
                    |> Zipper.previous
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 3)
        ]
