module Satisfactory exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as D exposing (Decoder, float, list, map, string)
import Json.Decode.Extra as DE
import Json.Decode.Pipeline exposing (required)
import Utils


type alias ItemId =
    String


type alias Item =
    { id : ItemId, name : String }


type Rate
    = PerMinute Float


type alias Production =
    { item : Item
    , rate : Rate
    }


type alias RecipeId =
    String


type alias Recipe =
    { id : RecipeId
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
    , recipe : Dict ItemId Item -> Decoder Recipe
    }
decoders_ =
    let
        item : Decoder Item
        item =
            D.succeed Item
                |> required "id" string
                |> required "name" string

        production : Dict ItemId Item -> Decoder Production
        production items =
            D.succeed Production
                |> required "item" (string |> Utils.decodeFromDict items)
                |> required "rate" (float |> D.map PerMinute)

        recipe : Dict ItemId Item -> Decoder Recipe
        recipe items =
            D.succeed Recipe
                |> required "id" string
                |> required "name" string
                |> required "inputs" (list <| production items)
                |> required "outputs" (list <| production items)
    in
    { item = item
    , recipe = recipe
    }
