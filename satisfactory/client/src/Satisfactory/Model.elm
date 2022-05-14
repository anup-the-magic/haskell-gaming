module Satisfactory.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (search)
import Satisfactory exposing (Recipe, RecipeId)


type SelectState a
    = Empty
    | Selected a
    | Open


selected : SelectState a -> Maybe a
selected s =
    case s of
        Selected a ->
            Just a

        Empty ->
            Nothing

        Open ->
            Nothing


type alias Model =
    { recipes : Dict RecipeId { amount : Float, recipe : Recipe }
    , addItem : SelectState Recipe
    }


empty : Model
empty =
    { recipes = Dict.empty
    , addItem = Empty
    }
