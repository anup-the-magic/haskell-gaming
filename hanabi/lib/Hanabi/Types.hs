module Hanabi.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Aeson.Types   (ToJSONKey (..), toJSONKeyText)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map           (Map)
import           Data.Set           (Set)
import qualified Data.Text          as Text
import           GHC.Generics       (Generic)

newtype PlayerId =
  PlayerId String
  deriving (Eq, Show, Ord, Generic)

newtype CardId =
  CardId String
  deriving (Eq, Show, Ord, Generic)

instance ToJSON PlayerId

instance ToJSON CardId

instance FromJSON PlayerId

instance FromJSON CardId

data ClueType
  = RankClue Rank
  | ColorClue Color
  deriving (Eq, Show)

data Move
  = Clue ClueType
         PlayerId
         (Set CardId)
  | Discard CardId
  | Play CardId
  deriving (Eq, Show)

data MoveOutcome
  = Win GameState
  | Loss GameState
  | Continue GameState
  deriving (Eq, Show)

data Err
  = OutOfTurn
  | InvalidMove
  | CardNotOwned
  | PlayerNotFound PlayerId
  | ClueNotComplete ClueType
                    PlayerId
                    (Set CardId)
  | NoCluesLeft
  | BrokenState
  | InvalidPlayerCount Int
  deriving (Eq, Show)

data Color
  = Red
  | Blue
  | Yellow
  | Green
  | White
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic) -- All needed for the table

instance ToJSON Color

instance ToJSONKey Color where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSON Color

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic) -- All needed for the table

instance ToJSON Rank

instance FromJSON Rank

data Clue
  = Rank
  | Color
  | Both
  deriving (Eq, Show, Generic)

instance ToJSON Clue

data Card = Card
  { cardId :: CardId
  , rank   :: Rank
  , color  :: Color
  , clues  :: Maybe Clue
  } deriving (Eq, Show, Generic)

instance ToJSON Card

data Player = Player
  { playerId :: PlayerId
  , hand     :: [Card]
  } deriving (Eq, Show)

data Lives
  = Hissss
  | Hisss
  | Hiss
  | Boom
  deriving (Eq, Show, Generic)

instance ToJSON Lives

data DeckState
  = Drawing (NonEmpty Card)
  | Ticking (NonEmpty Player)
  | NoCardsLeft
  deriving (Eq, Show)

data ClueTokens
  = Clue1
  | Clue2
  | Clue3
  | Clue4
  | Clue5
  | Clue6
  | Clue7
  | Clue8
  deriving (Eq, Show, Ord, Enum, Generic)

instance ToJSON ClueTokens

data GameState = GameState
  { players    :: NonEmpty Player
  , deck       :: DeckState
  , playables  :: Map Color (NonEmpty Rank)
  , lives      :: Lives
  , clueTokens :: Maybe ClueTokens
  , discards   :: [Card]
  } deriving (Eq, Show)
