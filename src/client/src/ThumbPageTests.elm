module ThumbPageTests exposing (..)

import ThumbPage exposing (..)
import Test exposing (..)
import Expect

testSpreadThumbs : Test
testSpreadThumbs =
    describe "spreadThumbs"
        [ test "images of the same height" <|
            \_ ->
                let
                    input = -- create input data
                    expectedOutput = -- define expected output
                in
                ThumbPage.spreadThumbs input
                    |> Expect.equal expectedOutput

        , test "images of different heights" <|
            \_ ->
                let
                    input = -- create input data
                    expectedOutput = -- define expected output
                in
                ThumbPage.spreadThumbs input
                    |> Expect.equal expectedOutput

        , test "images with combined width large enough to induce multiple images in the same column" <|
            \_ ->
                let
                    input = -- create input data
                    expectedOutput = -- define expected output
                in
                ThumbPage.spreadThumbs input
                    |> Expect.equal expectedOutput

        , test "images of different widths requiring scaling to fit the column width" <|
            \_ ->
                let
                    input = -- create input data
                    expectedOutput = -- define expected output
                in
                ThumbPage.spreadThumbs input
                    |> Expect.equal expectedOutput
        ]

all : Test
all =
    describe "ThumbPage Tests"
        [ testSpreadThumbs ]
