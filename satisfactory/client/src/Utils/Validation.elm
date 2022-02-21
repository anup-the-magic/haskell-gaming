module Utils.Validation exposing (..)

import Basics.Extra exposing (uncurry)
import Dict exposing (values)
import Result.Extra


type alias Validation error value =
    { value : value
    , errors : List error
    }


ok : a -> Validation error a
ok value =
    { errors = [], value = value }


accumulate : List (Validation err val) -> Validation err (List val)
accumulate vs =
    vs
        |> List.foldr (\v { errors, value } -> { errors = v.errors ++ errors, value = v.value :: value })
            (ok [])


fromResults : List (Result err val) -> Validation err (List val)
fromResults =
    Result.Extra.partition >> uncurry Validation


map : (a -> b) -> Validation err a -> Validation err b
map fn { value, errors } =
    { value = fn value, errors = errors }


map2 : (a -> b -> c) -> Validation err a -> Validation err b -> Validation err c
map2 fn v1 v2 =
    { value = fn v1.value v2.value, errors = v1.errors ++ v2.errors }


mapError : (a -> b) -> Validation a val -> Validation b val
mapError fn =
    mapErrors (List.map fn)


mapErrors : (List a -> List b) -> Validation a val -> Validation b val
mapErrors fn { errors, value } =
    { errors = fn errors, value = value }
