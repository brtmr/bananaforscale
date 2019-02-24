module BaseTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Notes
import Test exposing (..)


suite : Test
suite =
    describe "The Notes Module"
        [ describe "MIDI integers"
            [ test "0 to Octave" <|
                \_ ->
                    Expect.equal (Notes.midiToOctave 0) -1
            , test "11 to Octave" <|
                \_ ->
                    Expect.equal (Notes.midiToOctave 11) -1
            , test "12 to Octave" <|
                \_ ->
                    Expect.equal (Notes.midiToOctave 12) 0
            , test "15 to Pitch" <|
                \_ ->
                    Expect.equal (Notes.midiToPitch 15) 3
            , test "15 to Note Name" <|
                \_ ->
                    Expect.equal (Notes.midiToPitch 15 |> Notes.noteName) (Result.Ok ( "E♭", "D♯" ))
            ]
        ]
