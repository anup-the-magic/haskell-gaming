module Hanabi.Types where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map           (Map)
import           Hanabi.Deck        (Color (..), Rank (..))
import qualified Hanabi.Deck        as Deck

newtype PlayerId =
  PlayerId String
  deriving (Eq, Show, Ord)

newtype CardId =
  CardId String
  deriving (Eq, Show, Ord)

data Move
  = ColorClue PlayerId
              Color
              [Card]
  | RankClue PlayerId
             Rank
             [Card]
  | Discard Card
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
  | BrokenState
  deriving (Eq, Show)

data Card = Card
  { cardId :: CardId
  , card   :: Deck.Card
  } deriving (Eq, Show)

data Player = Player
  { playerId :: PlayerId
  , hand     :: [Card]
  } deriving (Eq, Show)

data Lives
  = Hissss
  | Hisss
  | Hiss
  | Boom
  deriving (Eq, Show)

data DeckState
  = Drawing (NonEmpty Card)
  | Ticking (NonEmpty Player)
  | NoCardsLeft
  deriving (Eq, Show)

data GameState = GameState
  { players   :: NonEmpty Player
  , deck      :: DeckState
  , playables :: Map Deck.Color [Deck.Rank]
  , lives     :: Lives
  } deriving (Eq, Show)
