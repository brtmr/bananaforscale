module BaseTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Maybe
import Notes
import Result
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
                    Expect.equal (Notes.midiToPitch 15) Notes.DSharp
            , test "15 to Note Name" <|
                \_ ->
                    Expect.equal (Notes.midiToPitch 15 |> Notes.noteName) ( "E♭", "D♯" )
            ]
        , describe "Note Names and Integers 0..11"
            [ test "0 represents C" <|
                \_ ->
                    Expect.equal 0 (Notes.noteToInt Notes.C)
            , test
                "11 represents B"
              <|
                \_ ->
                    Expect.equal 11 (Notes.noteToInt Notes.B)
            , test
                "C sharp and D flat are the same note"
              <|
                \_ ->
                    Expect.equal
                        ( Notes.sharp "C", Notes.flat "D" )
                        (Notes.noteName <| Notes.toNote <| Notes.sharp "C")
            , describe "Scales"
                [ test "The major scale consists of exactly 12 semitones" <|
                    \_ ->
                        Expect.equal
                            12
                            (List.sum (List.map Notes.scaleStepAsSemitones Notes.majorScale))
                ]
            ]
        ]
