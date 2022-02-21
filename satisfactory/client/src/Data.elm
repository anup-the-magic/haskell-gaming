module Data exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Http exposing (expectJson)
import Json.Decode as Decode exposing (list)
import List.Extra
import Satisfactory exposing (Item, ItemId, Recipe, RecipeId, decoders_)
import Set
import Utils exposing (MissingKeyError)
import Utils.Validation as Validation exposing (Validation)


type alias Items =
    Dict ItemId Item


type alias Recipes =
    Dict RecipeId Recipe


type alias Data =
    { items : Items, recipes : Recipes }


type InFlight a
    = Unfetched
    | Fetched a
    | Error Http.Error (List Http.Error)


empty : InFlight a
empty =
    Unfetched


fetchItems : Cmd (Result Http.Error Items)
fetchItems =
    Http.get
        { url = "http://localhost:8080/update-5/items.json"
        , expect =
            expectJson identity
                (list decoders_.item
                    |> Decode.map (Dict.Extra.fromListBy .id)
                )
        }



-- TODO: Convert to Http.task, to allow parallelization + merging


fetchRecipes : Dict ItemId Item -> Cmd (Result Http.Error (Validation MissingKeyError Recipes))
fetchRecipes items =
    let
        concatRecipes rs =
            rs
                |> (\{ errs, recipes } r ->
                        { errs = errs |> Set.fromList |> Set.toList
                        , recipes = recipes |> Dict.Extra.fromListBy .id
                        }
                   )
    in
    Http.get
        { url = "http://localhost:8080/update-5/recipes.json"
        , expect =
            expectJson identity
                (list (decoders_.recipe items)
                    |> Decode.map Validation.accumulate
                    |> Decode.map (Validation.map <| Dict.Extra.fromListBy .id)
                    |> Decode.map (Validation.mapErrors List.Extra.unique)
                )
        }
