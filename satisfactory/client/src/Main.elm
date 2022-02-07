module Main exposing (main)

import Browser exposing (Document)
import Data exposing (Data(..))
import Dict exposing (Dict)
import Element exposing (Element, column, html, row, text)
import Html exposing (Html)
import Html.Styled exposing (toUnstyled)
import Msg exposing (Msg(..))
import Satisfactory exposing (Factory)
import Satisfactory.View


type alias Model =
    { factory : Factory
    }


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
    ( { factory = Satisfactory.factory_.empty }
    , Cmd.none
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

            FetchedItems items ->
                text "Fetched Items!"

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
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
