module NeckNotes exposing (Tuning(..))

{-| We have defined all the functions and types necessary to define and calculate
notes and scales in the Notes module.

This module contains functions and types necessary to put these notes and
scales onto the guitar Neck

-}

import Dict exposing (..)
import Notes exposing (..)


{-| A Tuning is going to be 6 Integer values, which represent notes in MIDI
notation. For details see the Notes module.
-}
type Tuning
    = Tuning SPN SPN SPN SPN SPN SPN


{-| Tunings that will be available from the Interface

Taken from here:
<https://en.wikipedia.org/wiki/Guitar_tunings>

-}
standardTuning : Tuning
standardTuning =
    Tuning (SPN E 2) (SPN A 2) (SPN D 3) (SPN G 3) (SPN B 3) (SPN E 4)
