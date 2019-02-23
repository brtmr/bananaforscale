module Notes exposing (intervalName, midiToOctave, midiToPitch, noteName)

import Dict exposing (..)
import Result exposing (..)



-- This module contains all the music theory necessary to calculate
-- Minor, Major and Pentatonic scales
--
-- We are going to represent the fact that some notes have two names
-- (a flat and a sharp version) by having the noteName be a tuple.
-- This way, if a note has only a single name it will be a tuple of identical
-- Strings.


noteNames : Dict Int ( String, String )
noteNames =
    Dict.fromList
        [ ( 0, ( "C", "C" ) )
        , ( 1, ( "C♯", "D♭" ) )
        , ( 2, ( "D", "D" ) )
        , ( 3, ( "E♭", "D♯" ) )
        , ( 4, ( "E", "E" ) )
        , ( 5, ( "F", "F" ) )
        , ( 6, ( "F♯", "G♭" ) )
        , ( 7, ( "G", "G" ) )
        , ( 8, ( "A♭", "G♯" ) )
        , ( 9, ( "A", "A" ) )
        , ( 10, ( "B♭", "A♯" ) )
        , ( 11, ( "B", "B" ) )
        ]


noteName : Int -> Result String ( String, String )
noteName n =
    case Dict.get n noteNames of
        Just name ->
            Result.Ok name

        Nothing ->
            Result.Err "note is not in the Interval 0..11"



-- Maps the number of semitones in an interval to its name.
-- The IntervalNames are the Names referring to the major/minor scale, where
-- an interval is either minor/major or perfect.
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals


intervalNames : Dict Int String
intervalNames =
    Dict.fromList
        [ ( 0, "Perfect unison" )
        , ( 1, "Minor second" )
        , ( 2, "Major second" )
        , ( 3, "Minor third" )
        , ( 4, "Major third" )
        , ( 5, "Perfect fourth" )
        , ( 7, "Perfect fifth" )
        , ( 8, "Minor sixth" )
        , ( 9, "Major sixth" )
        , ( 10, "Minor seventh" )
        , ( 11, "Major seventh" )
        , ( 12, "Perfect octave" )
        ]


intervalName : Int -> Result String String
intervalName n =
    case Dict.get n intervalNames of
        Just name ->
            Result.Ok name

        Nothing ->
            Result.Err "not a named interval"



-- to represent notes we will use MIDI integer representation
-- https://en.wikipedia.org/wiki/Scientific_pitch_notation#Table_of_note_frequencies
-- 12 semi tones in an octave:


midiToOctave : Int -> Int
midiToOctave n =
    (n // 12) - 1



-- returns an index into the noteNames list corresponding to the pitch of
-- this MIDI number.


midiToPitch : Int -> Int
midiToPitch n =
    modBy 12 n
