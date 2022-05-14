module Utils exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as Decode exposing (Decoder, list)
import Json.Decode.Extra exposing (fromResult)
import Result exposing (fromMaybe)


type MissingKeyError
    = MissingKeyError String


dictFromListKeeping : List ( comparable, v ) -> Dict comparable (List v)
dictFromListKeeping =
    dictFromListKeepingBy identity


dictFromListKeepingBy : (a -> ( comparable, v )) -> List a -> Dict comparable (List v)
dictFromListKeepingBy fn xs =
    let
        prependOrInsert x existing =
            existing
                |> Maybe.map ((::) x)
                |> Maybe.withDefault [ x ]
                |> Just
    in
    xs
        |> List.foldr
            -- Convert to (k, v), then insert or update
            (fn >> (\( k, x ) -> Dict.update k <| prependOrInsert x))
            Dict.empty


decodeFromDict : Dict String a -> Decoder String -> Decoder (Result MissingKeyError a)
decodeFromDict dict key =
    let
        keys =
            Dict.keys dict |> String.join ",\n\t"

        decodeValue : String -> Result MissingKeyError a
        decodeValue k =
            dict
                |> Dict.get k
                |> fromMaybe (MissingKeyError k)
    in
    key
        |> Decode.map decodeValue


decodeFromDict_ : Dict String a -> Decoder String -> Decoder a
decodeFromDict_ dict key =
    let
        keys =
            Dict.keys dict |> String.join ",\n\t"

        errMsg (MissingKeyError k) =
            "missing key "
                ++ k
                ++ " in Dict. Existing keys: [\n"
                ++ keys
                ++ "\n]"
    in
    decodeFromDict dict key
        |> Decode.map (Result.mapError errMsg)
        |> Decode.andThen fromResult


filter : (a -> Bool) -> a -> Maybe a
filter fn x =
    if fn x then
        Just x

    else
        Nothing
