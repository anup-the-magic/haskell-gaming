module Hanabi.Types where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map           (Map)
import           Data.Set           (Set)

newtype PlayerId =
  PlayerId String
  deriving (Eq, Show, Ord)

newtype CardId =
  CardId String
  deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Ord, Enum)

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Eq, Show, Ord, Enum)

data Clue
  = Rank
  | Color
  | Both
  deriving (Eq, Show)

data Card = Card
  { cardId :: CardId
  , rank   :: Rank
  , color  :: Color
  , clues  :: Maybe Clue
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

data ClueTokens
  = Clue1
  | Clue2
  | Clue3
  | Clue4
  | Clue5
  | Clue6
  | Clue7
  | Clue8
  deriving (Eq, Show, Ord, Enum)

data GameState = GameState
  { players    :: NonEmpty Player
  , deck       :: DeckState
  , playables  :: Map Color (NonEmpty Rank)
  , lives      :: Lives
  , clueTokens :: Maybe ClueTokens
  , discards   :: [Card]
  } deriving (Eq, Show)
