module Main exposing (main)

import Element exposing (row, text)
import Html exposing (Html)


main : Html msg
main =
    Element.layout [] <|
        row [] [ text "foobar" ]
