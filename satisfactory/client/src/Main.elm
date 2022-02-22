module Main exposing (main)

import Browser exposing (Document)
import Data exposing (Data, InFlight(..))
import Dict exposing (Dict)
import Element exposing (Element, column, html, row, text)
import Http
import Model exposing (Model)
import Msg exposing (Msg(..))
import Satisfactory exposing (Factory, Item, Recipe)
import Satisfactory.View
import Utils exposing (MissingKeyError)
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
    ( { factory = Satisfactory.factory_.empty
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
