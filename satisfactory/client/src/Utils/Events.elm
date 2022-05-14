module Utils.Events exposing (..)

import Element exposing (htmlAttribute)
import Html.Events as HE
import Json.Decode as Decode


onClickWithOpts : { stopPropagation : Bool, preventDefault : Bool, message : msg } -> Element.Attribute msg
onClickWithOpts =
    Decode.succeed
        >> HE.custom "click"
        >> htmlAttribute


{-| On click, but stop propagation. Useful if you plan to attach a Blur to subscriptions
-}
onClick_ : msg -> Element.Attribute msg
onClick_ msg =
    onClickWithOpts
        { message = msg
        , stopPropagation = True
        , preventDefault = False
        }
