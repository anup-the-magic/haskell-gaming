module View exposing (..)

import Data exposing (Data, InFlight(..))
import Element exposing (Element, alignLeft, alignRight, column, fill, height, html, px, rgb, rgb255, row, text, width)
import Element.Background as Background
import Http
import Model exposing (Model)
import Msg exposing (Msg)
import Satisfactory.View
import Utils exposing (MissingKeyError(..))


body : Model -> Element Msg
body model =
    column [ width fill, height fill ]
        [ header
        , content model
        ]


header : Element Msg
header =
    row [ height (px 30) ]
        [ text "Satisfactory Calculator!"
        ]


content : Model -> Element Msg
content model =
    case model.data of
        Unfetched ->
            text "Fetching!"

        Fetched data ->
            row [ width fill ]
                [ Satisfactory.View.view model.factory data
                    |> Element.el [ alignLeft ]
                , column [ alignRight ]
                    [ text ("Missing Items: " ++ (String.fromInt <| List.length model.errors))
                    , model.errors
                        |> List.map (\(MissingKeyError err) -> text err)
                        |> column [ alignRight ]
                    ]
                ]

        Error e errs ->
            column [ Background.color <| rgb255 200 0 0 ]
                ((e :: errs) |> List.map httpError)


httpError : Http.Error -> Element Msg
httpError e =
    case e of
        Http.BadBody msg ->
            text msg

        _ ->
            text <| Debug.toString e
