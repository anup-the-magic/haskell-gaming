module Msg exposing (Msg(..))

import Data exposing (Data)
import Dict exposing (Dict)
import Http
import Satisfactory exposing (Item, ItemId, Recipe, RecipeId)
import Utils exposing (MissingKeyError)
import Utils.Validation exposing (Validation)


type Msg
    = ReceivedItems (Result Http.Error Data.Items)
    | ReceivedRecipes Data.Items (Result Http.Error (Validation MissingKeyError Data.Recipes))
