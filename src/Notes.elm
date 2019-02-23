module Notes exposing (noteNames)

-- This module contains all the music theory necessary to calculate
-- Minor, Major and Pentatonic scales
--
-- We are going to represent the fact that some notes have two names
-- (a flat and a sharp version) by having the noteName be a tuple.
-- This way, if a note has only a single name it will be a tuple of identical
-- Strings.


noteNames : List ( String, String )
noteNames =
    [ ( "C", "C" )
    , ( "C♯", "D♭" )
    , ( "D", "D" )
    , ( "E♭", "D♯" )
    , ( "E", "E" )
    , ( "F", "F" )
    , ( "F♯", "G♭" )
    , ( "G", "G" )
    , ( "A♭", "G♯" )
    , ( "A", "A" )
    , ( "B♭", "A♯" )
    , ( "B", "B" )
    ]



-- Maps the number of semitones in an interval to its name.
-- The IntervalNames are the Names referring to the major/minor scale, where
-- an interval is either minor/major or perfect.


intervalName : Int -> Result String String
intervalName 0 =
    Ok "Perfect unison"
intervalName 1 =
    Ok "Minor second"
intervalName 2 =
    Ok "Major second"
intervalName 3 =
    Ok "Minor third"
intervalName 4 =
    Ok "Major third"
intervalName 5 =
    Ok "Perfect fourth"
intervalName 7 =
    Ok "Perfect fifth"
intervalName 8 =
    Ok "Minor sixth"
intervalName 9 =
    Ok "Major sixth"
intervalName 10 =
    Ok "Minor seventh"
intervalName 11 =
    Ok "Major seventh"
intervalName 12 =
    Ok "Perfect octave"
intervalName n =
    Error "not an interval name"
