module Notes exposing
    ( flat
    , intervalName
    , midiToOctave
    , midiToPitch
    , noteName
    , noteNameToInt
    , sharp
    , toFullNoteName
    )

import Dict exposing (..)
import Maybe



-- We are going to use the unicode sharp and flat symbols. To avoid having
-- to copy and paste them around, or dealing with escape sequences all over
-- the code, the following two functions append them to any given string


sharp : String -> String
sharp s =
    s ++ "♯"


flat : String -> String
flat s =
    s ++ "♭"



-- This module contains all the music theory necessary to calculate
-- Minor, Major and Pentatonic scales
--
-- We are going to represent the fact that some notes have two names
-- (a flat and a sharp version) by having the noteName be a tuple.
-- This way, if a note has only a single name it will be a tuple of identical
-- Strings.


type alias NoteName =
    ( String, String )


noteNames : Dict Int NoteName
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



-- we want to be able to easily get the tuple for every note String,
-- so that we can generate the tuple "C♯", "D♭" from just one of its
-- components.
-- lets define a convience function, that given a condition will return
-- the first item in a list that matches that condition.


filterFirst : List a -> (a -> Bool) -> Maybe a
filterFirst xs p =
    case xs of
        [] ->
            Maybe.Nothing

        first :: rest ->
            if p first then
                Maybe.Just first

            else
                filterFirst rest p


toFullNoteName : String -> Maybe NoteName
toFullNoteName s =
    let
        nameTuples =
            Dict.values noteNames

        matchAny =
            \t -> (s == Tuple.first t) || (s == Tuple.second t)
    in
    filterFirst nameTuples matchAny


noteName : Int -> Maybe NoteName
noteName n =
    Dict.get n noteNames


noteNameToInt : NoteName -> Maybe Int
noteNameToInt name =
    let
        namesAsList =
            Dict.toList noteNames

        matchValue =
            \t -> name == Tuple.second t
    in
    Maybe.map Tuple.first (filterFirst namesAsList matchValue)



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


intervalName : Int -> Maybe String
intervalName n =
    Dict.get n intervalNames



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



-- Let's make some scales!
