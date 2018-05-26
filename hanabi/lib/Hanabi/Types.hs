module Hanabi.Types where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map           (Map)

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

data Color
  = Red
  | Blue
  | Yellow
  | Green
  | White
  deriving (Ord, Enum, Eq, Show)

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Ord, Enum, Eq, Show)

data Card = Card
  { cardId :: CardId
  , rank   :: Rank
  , color  :: Color
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
  , playables :: Map Color [Rank]
  , lives     :: Lives
  } deriving (Eq, Show)
