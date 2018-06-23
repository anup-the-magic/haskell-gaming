module Dummy where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Json

payload :: Json.Value
payload = Json.object ["deck" .= deck, "players" .= players]

deck :: Json.Value
deck = Json.object ["cardsRemaining" .= (22 :: Int)]

players :: Json.Value
players = undefined
