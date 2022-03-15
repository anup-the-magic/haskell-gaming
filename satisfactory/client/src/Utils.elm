module Utils exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as Decode exposing (Decoder, list)
import Json.Decode.Extra exposing (fromResult)
import Result exposing (fromMaybe)


decodeFromDict : Dict String a -> Decoder String -> Decoder a
decodeFromDict dict key =
    let
        keys =
            Dict.keys dict |> String.join ",\n\t"

        errMsg k =
            "missing key "
                ++ k
                ++ " in Dict. Existing keys: [\n"
                ++ keys
                ++ "\n]"

        decodeValue k =
            dict
                |> Dict.get k
                |> fromMaybe (errMsg k)
                |> fromResult
    in
    key
        |> Decode.andThen decodeValue


listKeyedBy : (a -> comparable) -> Decoder a -> Decoder (Dict comparable a)
listKeyedBy keyFn decoder =
    list decoder
        |> Decode.map (Dict.Extra.fromListBy keyFn)
