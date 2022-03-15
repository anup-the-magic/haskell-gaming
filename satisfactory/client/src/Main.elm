module Main exposing (main)

import Browser exposing (Document)
import Data exposing (Data, InFlight(..))
import Dict exposing (Dict)
import Element exposing (Element, column, html, row, text)
import Html exposing (Html)
import Html.Styled exposing (toUnstyled)
import Http
import Model exposing (Model)
import Msg exposing (Msg(..))
import Satisfactory exposing (Factory, Item, Recipe)
import Satisfactory.View


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
      }
    , Data.fetchItems |> Cmd.map ReceivedItems
    )


view : Model -> Document Msg
view model =
    { title = "Satisfactory calculator!"
    , body = [ Element.layout [] <| body model ]
    }


body : Model -> Element Msg
body model =
    column []
        [ case model.data of
            Unfetched ->
                text "Fetching!"

            Fetched { items, recipes } ->
                Satisfactory.View.view model.factory
                    |> toUnstyled
                    |> html

            Error e errs ->
                column [] ((e :: errs) |> List.map viewError)
        ]


viewError : Http.Error -> Element Msg
viewError e =
    case e of
        Http.BadBody msg ->
            text msg

        _ ->
            text <| Debug.toString e


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

        ReceivedRecipes result ->
            case result of
                Err err ->
                    case model.data of
                        Error e es ->
                            ( { model | data = Error e (err :: es) }, Cmd.none )

                        _ ->
                            ( { model | data = Error err [] }, Cmd.none )

                Ok data ->
                    ( { model | data = data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
