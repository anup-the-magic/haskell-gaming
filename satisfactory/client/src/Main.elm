module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onClick)
import Data exposing (InFlight(..))
import Dict
import Element
import Element.Events as Events
import Http
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg(..))
import Satisfactory
import Satisfactory.Model as Satisfactory exposing (SelectState(..))
import View


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { factory = Satisfactory.empty
      , data = Data.empty
      , errors = []
      }
    , Data.fetchItems |> Cmd.map ReceivedItems
    )


view : Model -> Document Msg
view model =
    { title = "Satisfactory calculator!"
    , body = [ Element.layout [] <| View.body model ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedItems result ->
            case result of
                Err err ->
                    case model.data of
                        Error e es ->
                            ( { model | data = Error e (err :: es) }, Cmd.none )

                        _ ->
                            ( { model | data = Error err [] }, Cmd.none )

                Ok items ->
                    ( model
                    , Data.fetchRecipes items |> Cmd.map (ReceivedRecipes items)
                    )

        ReceivedRecipes items result ->
            case result of
                Err err ->
                    case model.data of
                        Error e es ->
                            ( { model | data = Error e (err :: es) }, Cmd.none )

                        _ ->
                            ( { model | data = Error err [] }, Cmd.none )

                Ok recipes ->
                    ( { model
                        | data = Fetched { items = items, recipes = recipes.value }
                        , errors = recipes.errors
                      }
                    , Cmd.none
                    )

        AddToFactory recipe ->
            let
                f =
                    model.factory

                updatedRecipes =
                    model.factory.recipes
                        |> Dict.insert recipe.id { amount = 1, recipe = recipe }

                model_ =
                    { model | factory = { f | recipes = updatedRecipes } }
            in
            model_
                |> update Msg.Blur

        OpenRecipeSelector ->
            let
                f =
                    model.factory
            in
            ( { model | factory = { f | addItem = Open } }, Cmd.none )

        Blur ->
            let
                f =
                    model.factory
            in
            ( { model | factory = { f | addItem = Empty } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { factory } =
    case factory.addItem of
        Open ->
            onClick (Decode.succeed Msg.Blur)

        _ ->
            Sub.none
