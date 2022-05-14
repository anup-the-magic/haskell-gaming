module Model exposing (..)

import Data exposing (Data, InFlight)
import Satisfactory.Model as Satisfactory
import Utils exposing (MissingKeyError)


type alias Model =
    { factory : Satisfactory.Model
    , data : InFlight Data
    , errors : List MissingKeyError
    }
