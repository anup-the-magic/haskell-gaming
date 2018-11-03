module Main exposing (main)

import Browser
import Html exposing (div)


main : Program () Int msg
main =
    Browser.sandbox
        { init = 0
        , view = always <| div [] []
        , update = \_ _ -> 0
        }
