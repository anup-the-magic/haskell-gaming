module Satisfactory exposing (..)

import Html exposing (Html)


foobar : Int
foobar =
    1


type alias Resource =
    { name : String
    , id : Int
    }


type alias Production =
    { resource : Resource
    , amount : Float
    }


view : Production -> Html msg
view =
    Debug.todo "Create view function for a production"
