module Model exposing (..)

import Data exposing (Data, InFlight)
import Satisfactory exposing (Factory)
import Utils exposing (MissingKeyError)


type alias Model =
    { factory : Factory
    , data : InFlight Data
    , errors : List MissingKeyError
    }
