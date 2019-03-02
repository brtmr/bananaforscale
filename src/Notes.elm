module Notes exposing (Note(..), NoteName, SPN(..), Scale, ScaleStep(..), filterFirst, flat, intToNote, intervalName, majorScale, makeScale, midiToOctave, midiToPitch, midiToSPN, minorScale, noteName, noteToInt, scaleStepAsSemitones, sharp, toNote)

import Dict exposing (..)
import Maybe



-- This module contains all the music theory necessary to calculate
-- Minor, Major and Pentatonic scales
--
-- We are going to represent the fact that some notes have two names
-- (a flat and a sharp version) by having the noteName be a tuple.
-- This way, if a note has only a single name it will be a tuple of identical
-- Strings.
-- We are going to use the unicode sharp and flat symbols. To avoid having
-- to copy and paste them around, or dealing with escape sequences all over
-- the code, the following two functions append them to any given string


sharp : String -> String
sharp s =
    s ++ "♯"


flat : String -> String
flat s =
    s ++ "♭"


type Note
    = C
    | CSharp
    | D
    | DSharp
    | E
    | F
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B


type alias NoteName =
    ( String, String )


noteName : Note -> NoteName
noteName n =
    case n of
        C ->
            ( "C", "C" )

        CSharp ->
            ( "C♯", "D♭" )

        D ->
            ( "D", "D" )

        DSharp ->
            ( "E♭", "D♯" )

        E ->
            ( "E", "E" )

        F ->
            ( "F", "F" )

        FSharp ->
            ( "F♯", "G♭" )

        G ->
            ( "G", "G" )

        GSharp ->
            ( "A♭", "G♯" )

        A ->
            ( "A", "A" )

        ASharp ->
            ( "B♭", "A♯" )

        B ->
            ( "B", "B" )



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


toNote : String -> Note
toNote s =
    case s of
        "C" ->
            C

        "C♯" ->
            CSharp

        "D♭" ->
            CSharp

        "D" ->
            D

        "E♭" ->
            DSharp

        "D♯" ->
            DSharp

        "E" ->
            E

        "F" ->
            F

        "F♯" ->
            FSharp

        "G♭" ->
            FSharp

        "G" ->
            G

        "A♭" ->
            GSharp

        "G♯" ->
            GSharp

        "A" ->
            A

        "B♭" ->
            ASharp

        "A♯" ->
            ASharp

        "B" ->
            B

        _ ->
            C


noteToInt : Note -> Int
noteToInt n =
    case n of
        C ->
            0

        CSharp ->
            1

        D ->
            2

        DSharp ->
            3

        E ->
            4

        F ->
            5

        FSharp ->
            6

        G ->
            7

        GSharp ->
            8

        A ->
            9

        ASharp ->
            10

        B ->
            11


intToNote : Int -> Note
intToNote n =
    case n of
        0 ->
            C

        1 ->
            CSharp

        2 ->
            D

        3 ->
            DSharp

        4 ->
            E

        5 ->
            F

        6 ->
            FSharp

        7 ->
            G

        8 ->
            GSharp

        9 ->
            A

        10 ->
            ASharp

        11 ->
            B

        _ ->
            intToNote <| modBy 12 n



-- Maps the number of semitones in an interval to its name.
-- The IntervalNames are the Names referring to the major/minor scale, where
-- an interval is either minor/major or perfect.
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals


intervalName : Int -> String
intervalName i =
    case i of
        0 ->
            "Perfect unison"

        1 ->
            "Minor second"

        2 ->
            "Major second"

        3 ->
            "Minor third"

        4 ->
            "Major third"

        5 ->
            "Perfect fourth"

        7 ->
            "Perfect fifth"

        8 ->
            "Minor sixth"

        9 ->
            "Major sixth"

        10 ->
            "Minor seventh"

        11 ->
            "Major seventh"

        12 ->
            "Perfect octave"

        _ ->
            "Unnamed Interval"



-- to represent notes we will use MIDI integer representation
-- https://en.wikipedia.org/wiki/Scientific_pitch_notation#Table_of_note_frequencies
-- 12 semi tones in an octave:


midiToOctave : Int -> Int
midiToOctave n =
    (n // 12) - 1



-- returns an index into the noteNames list corresponding to the pitch of
-- this MIDI number.


midiToPitch : Int -> Note
midiToPitch n =
    intToNote <| modBy 12 n



-- Finally, we want to express Midi notes in Scientific Pitch Notation,
-- which is the combination of the Name of the note plus a positive
-- integer representing the octave of the note.


type SPN
    = SPN Note Int


midiToSPN : Int -> SPN
midiToSPN n =
    SPN (midiToPitch n) (midiToOctave n)



-- Let's make some scales!
-- First we need some types:


type ScaleStep
    = Half
    | Whole
    | WholeAndAHalf


scaleStepAsSemitones : ScaleStep -> Int
scaleStepAsSemitones step =
    case step of
        Half ->
            1

        Whole ->
            2

        WholeAndAHalf ->
            3


type alias Scale =
    List ScaleStep



-- Now we have everything in Place to define our scales as a number of steps


majorScale : Scale
majorScale =
    [ Whole
    , Whole
    , Half
    , Whole
    , Whole
    , Whole
    , Half
    ]


minorScale : Scale
minorScale =
    [ Whole
    , Half
    , Whole
    , Whole
    , Half
    , Whole
    , Whole
    ]


makeStep : Note -> ScaleStep -> Note
makeStep n s =
    intToNote <| noteToInt n + scaleStepAsSemitones s


makeScale : Note -> Scale -> List Note
makeScale root scale =
    case scale of
        step :: rest ->
            let
                next =
                    makeStep root step
            in
            root :: makeScale next rest

        [] ->
            []
