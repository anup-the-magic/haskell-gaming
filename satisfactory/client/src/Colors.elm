module Colors exposing (..)

import Element exposing (Color)


type alias Colors a =
    { background :
        { light : a
        , dark : a
        }
    , foregrond :
        { light : a
        , dark : a
        }
    , font :
        { light : a
        , dark : a
        }
    , highlight :
        { light : a
        , dark : a
        }
    }


type alias ColorScheme =
    Colors Color


type alias ExplainColorScheme =
    Colors (List String)
