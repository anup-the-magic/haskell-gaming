module Types where

import           Data.Aeson         (ToJSON (..), object, (.=))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map           (Map)
import           GHC.Generics       (Generic)
import           Hanabi.Types       (Card (..), CardId (..), ClueTokens (..),
                                     Color (..), Lives (..), Player (..),
                                     PlayerId (..), Rank (..))

data Turns
  = Drawing Int
  | Ticking Int
  deriving (Show, Eq, Generic)

instance ToJSON Turns

data GameState = GameState
  { turns         :: Turns
  , currentPlayer :: PlayerId
  , playables     :: Map Color (NonEmpty Rank)
  , lives         :: Lives
  , clueTokens    :: Maybe ClueTokens
  , discards      :: [Card]
  } deriving (Show, Eq, Generic)

instance ToJSON GameState

data FilteredCard
  = Unknown { cardId :: CardId }
  | Rank { cardId :: CardId
         , rank   :: Rank }
  | Color { cardId :: CardId
          , color  :: Color }
  | Both { cardId :: CardId
         , rank   :: Rank
         , color  :: Color }

instance ToJSON FilteredCard where
  toJSON (Unknown {Types.cardId = cardId}) =
    object ["type_" .= ("unknown" :: String), "cardId" .= cardId]
  toJSON (Types.Rank {Types.cardId = cardId, Types.rank = rank}) =
    object ["type_" .= ("rank" :: String), "cardId" .= cardId, "rank" .= rank]
  toJSON (Types.Color {Types.cardId = cardId, Types.color = color}) =
    object
      ["type_" .= ("color" :: String), "cardId" .= cardId, "color" .= color]
  toJSON (Types.Both { Types.cardId = cardId
                     , Types.rank = rank
                     , Types.color = color
                     }) =
    object
      [ "type_" .= ("both" :: String)
      , "cardId" .= cardId
      , "rank" .= rank
      , "color" .= color
      ]

data FilteredPlayer = FilteredPlayer
  { playerId :: PlayerId
  , cards    :: [FilteredCard]
  }

data CurrentGame = CurrentGame
  { gameState :: GameState
  , you       :: FilteredPlayer
  , players   :: NonEmpty Player
  }
