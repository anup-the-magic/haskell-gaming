module Satisfactory.View exposing (view)

import Element exposing (Element, column, row, text)
import Html
import List
import Msg exposing (Msg)
import Satisfactory exposing (Factory, Recipe)
import String


viewRecipe : Recipe -> Element Msg
viewRecipe recipe =
    row [] [ text recipe.name ]


view : Factory -> Element Msg
view factory =
    column []
        [ row [] [ text "Basic Factory" ]
        , factory.recipes
            |> List.map
                (\{ amount, recipe } ->
                    row [] [ text <| String.fromFloat amount, viewRecipe recipe ]
                )
            |> column []
        ]
