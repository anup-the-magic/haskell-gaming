module Satisfactory exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as D exposing (Decoder, float, list, map, string)
import Json.Decode.Extra as DE
import Json.Decode.Pipeline exposing (required)
import List.Nonempty exposing (Nonempty)
import Result.Extra
import Utils exposing (MissingKeyError)
import Utils.Validation as Validation exposing (Validation)


type alias ItemId =
    String


type alias Item =
    { id : ItemId, name : String }


type Rate
    = PerMinute Float


type alias Production =
    { rate : Rate
    , item : Item
    }


type alias RecipeId =
    String


type alias Recipe =
    { id : RecipeId
    , name : String
    , inputs : List Production
    , outputs : List Production
    }


type alias Factory =
    { recipes : List { amount : Float, recipe : Recipe } }


factory_ : { empty : Factory }
factory_ =
    { empty = { recipes = [] } }


decoders_ :
    { item : Decoder Item
    , recipe : Dict ItemId Item -> Decoder (Validation MissingKeyError Recipe)
    }
decoders_ =
    let
        item : Decoder Item
        item =
            D.succeed Item
                |> required "id" string
                |> required "name" string

        production : Dict ItemId Item -> Decoder (Result MissingKeyError Production)
        production items =
            D.succeed (\rate -> Result.map (Production rate))
                |> required "rate" (float |> D.map PerMinute)
                |> required "item" (string |> Utils.decodeFromDict items)

        recipe : Dict ItemId Item -> Decoder (Validation MissingKeyError Recipe)
        recipe items =
            D.succeed toRecipe
                |> required "id" string
                |> required "name" string
                |> required "inputs" (list <| production items)
                |> required "outputs" (list <| production items)

        toRecipe :
            RecipeId
            -> String
            -> List (Result MissingKeyError Production)
            -> List (Result MissingKeyError Production)
            -> Validation MissingKeyError Recipe
        toRecipe id name inputs outputs =
            Validation.map2 (Recipe id name)
                (inputs |> Validation.fromResults)
                (outputs |> Validation.fromResults)
    in
    { item = item
    , recipe = recipe
    }
