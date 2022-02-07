module Satisfactory exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as D exposing (Decoder, float, list, string)
import Json.Decode.Extra as DE
import Json.Decode.Pipeline exposing (required)
import Utils


type alias Item =
    { id : String, name : String }


type Rate
    = PerMinute Float


type alias Production =
    { item : Item
    , rate : Rate
    }


type alias Recipe =
    { id : String
    , name : String
    , inputs : List Production
    , output : List Production
    }


type alias Factory =
    { recipes : List { amount : Float, recipe : Recipe } }


factory_ : { empty : Factory }
factory_ =
    { empty = { recipes = [] } }


decoders_ :
    { item : Decoder Item
    , production : Dict String Item -> Decoder Production
    , recipe : Dict String Item -> Decoder Recipe
    }
decoders_ =
    let
        item : Decoder Item
        item =
            D.succeed Item
                |> required "id" string
                |> required "name" string

        production : Dict String Item -> Decoder Production
        production items =
            D.succeed Production
                |> required "item" (string |> D.andThen (Utils.decodeFromDict items))
                |> required "rate" (float |> D.map PerMinute)

        recipe : Dict String Item -> Decoder Recipe
        recipe items =
            D.succeed Recipe
                |> required "id" string
                |> required "name" string
                |> required "inputs" (list <| production items)
                |> required "outputs" (list <| production items)
    in
    { item = item
    , production = production
    , recipe = recipe
    }
