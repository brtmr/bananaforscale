module BaseTest exposing (neckNotes, notes)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Maybe
import NeckNotes
import Notes
import Result
import Test exposing (..)


neckNotes : Test
neckNotes =
    describe "The NeckNotes module"
        [ describe "Tunings"
            [ test "standard tuning in SPN" <|
                \_ -> Expect.equal 0 0
            ]
        ]


notes : Test
notes =
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
        , describe "Notes and Integers 0..11"
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
            ]
        , describe "midi and Scientific pitch notation"
            [ test "C_-1 is midi number 0" <|
                \_ ->
                    Expect.equal 0 (Notes.spnToMidi <| Notes.SPN Notes.C -1)
            , test "midi number 0 is C_-1" <|
                \_ ->
                    Expect.equal
                        (Notes.SPN Notes.C -1)
                        (0 |> Notes.midiToSPN)
            , test "C_1 is midi number 24" <|
                \_ ->
                    Expect.equal 24 (Notes.spnToMidi <| Notes.SPN Notes.C 1)
            , Test.fuzz int "midi to spn to midi is always the same number." <|
                \midi ->
                    Expect.equal
                        midi
                        (midi |> Notes.midiToSPN |> Notes.spnToMidi)
            , test "B_-2 (outside of midi) is -1" <|
                \_ ->
                    Expect.equal
                        (Notes.SPN Notes.B -2)
                        (-1 |> Notes.midiToSPN)
            , test "C_-2 (outside of midi) is -12" <|
                \_ ->
                    Expect.equal
                        (Notes.SPN Notes.C -2)
                        (-12 |> Notes.midiToSPN)
            ]
        , describe "Scales"
            [ test "The major scale consists of exactly 12 semitones" <|
                \_ ->
                    Expect.equal
                        12
                        (List.sum (List.map Notes.scaleStepAsSemitones Notes.majorScale))
            , test "C major scale" <|
                \_ ->
                    Expect.equal
                        (Notes.makeScale Notes.C Notes.majorScale)
                        [ Notes.C
                        , Notes.D
                        , Notes.E
                        , Notes.F
                        , Notes.G
                        , Notes.A
                        , Notes.B
                        ]
            ]
        ]
