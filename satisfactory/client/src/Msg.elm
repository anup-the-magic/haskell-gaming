module Msg exposing (Msg(..))

import Data exposing (Data)
import Dict exposing (Dict)
import Http
import Satisfactory exposing (Item, ItemId, Recipe, RecipeId)


type Msg
    = ReceivedItems (Result Http.Error Data.Items)
    | ReceivedRecipes (Result Http.Error Data)
