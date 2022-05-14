module Satisfactory.View exposing (view)

import Data exposing (Data, Recipes)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, column, el, fill, height, px, row, text, width)
import List
import Msg exposing (Msg)
import Satisfactory exposing (ItemId, Recipe)
import Satisfactory.Model as Model exposing (Model, SelectState(..))
import String
import Utils
import Utils.Events as Events exposing (onClick_)


type alias Sources =
    Dict ItemId (List Recipe)


sources_ : Recipes -> Sources
sources_ recipes =
    recipes
        |> Dict.values
        |> List.concatMap (\r -> r.inputs |> List.map (\i -> ( i.item.id, r )))
        |> Utils.dictFromListKeeping


view : Model -> Data -> Element Msg
view model { recipes, items } =
    column []
        [ row [] [ text "Basic Factory" ]
        , row [] [ text "Add a recipe", recipeSelector recipes model.addItem ]
        , model.recipes
            |> Dict.values
            |> List.map
                (\{ amount, recipe } ->
                    row [] [ text <| String.fromFloat amount, viewRecipe recipe ]
                )
            |> column []
        ]


recipeSelector : Recipes -> SelectState Recipe -> Element Msg
recipeSelector recipes state =
    let
        button =
            state
                |> Utils.filter ((/=) Open)
                |> Maybe.map
                    (\_ -> [ onClick_ Msg.OpenRecipeSelector ])
                |> Maybe.withDefault []
                |> (\attrs ->
                        el ([ width (px 200) ] ++ attrs)
                            (text "Pick your recipe")
                   )

        placeholder =
            state
                |> Model.selected
                |> Maybe.map .name
                |> Maybe.map (text >> el [])
                |> Maybe.withDefault
                    (el [ Events.onClick_ Msg.OpenRecipeSelector ]
                        (text "Pick your Recipe")
                    )

        dropdown =
            recipes
                |> Dict.values
                |> List.map
                    (\recipe ->
                        row [ Events.onClick_ (Msg.AddToFactory recipe) ]
                            [ text recipe.name ]
                    )
                |> column []
    in
    column
        [ width (px 200)
        , height fill
        , Element.below <|
            if state == Open then
                dropdown

            else
                Element.none
        ]
        [ placeholder ]


viewRecipe : Recipe -> Element Msg
viewRecipe recipe =
    row [] [ text recipe.name ]
