module Data exposing (..)

import Dict exposing (Dict)
import Http exposing (expectJson)
import Json.Decode as Decode
import Satisfactory exposing (Item, ItemId, Recipe, RecipeId, decoders_)
import Utils exposing (listKeyedBy)


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
                (decoders_.item |> listKeyedBy .id)
        }



-- TODO: Convert to Http.task, to allow parallelization + merging


fetchRecipes : Dict ItemId Item -> Cmd (Result Http.Error (InFlight Data))
fetchRecipes items =
    Http.get
        { url = "http://localhost:8080/update-5/recipes.json"
        , expect =
            expectJson identity
                (decoders_.recipe items
                    |> listKeyedBy .id
                    |> Decode.map
                        (\recipes -> Fetched { items = items, recipes = recipes })
                )
        }
