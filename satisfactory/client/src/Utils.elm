module Utils exposing (decodeFromDict)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra exposing (fromResult)
import Result exposing (fromMaybe)


decodeFromDict : Dict String a -> String -> Decoder a
decodeFromDict dict key =
    let
        keys =
            Dict.keys dict |> String.join ",\n\t"

        errMsg =
            "missing key "
                ++ key
                ++ " in Dict. Existing keys: [\n"
                ++ keys
                ++ "\n]"
    in
    dict
        |> Dict.get key
        |> fromMaybe errMsg
        |> fromResult
