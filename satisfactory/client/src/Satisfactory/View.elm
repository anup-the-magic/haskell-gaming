module Satisfactory.View exposing (view)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import List
import Msg exposing (Msg)
import Satisfactory exposing (Factory, Recipe)
import String


viewRecipe : Recipe -> Html Msg
viewRecipe recipe =
    div [ css [] ] [ text recipe.name ]


view : Factory -> Html Msg
view factory =
    div []
        [ div [] [ text "Basic Factory" ]
        , factory.recipes
            |> List.map
                (\{ amount, recipe } ->
                    div [] [ text <| String.fromFloat amount, viewRecipe recipe ]
                )
            |> div []
        ]
