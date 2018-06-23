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

colorStr :: Color -> String
colorStr Red    = "R"
colorStr Green  = "G"
colorStr Blue   = "B"
colorStr White  = "W"
colorStr Yellow = "Y"

rankStr :: Rank -> String
rankStr One   = "1"
rankStr Two   = "2"
rankStr Three = "3"
rankStr Four  = "4"
rankStr Five  = "5"

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
  } deriving (Eq, Show, ToJSON)

clueCard :: ClueType -> Card -> Card
clueCard clue card@Card {color = color, rank = rank, clues = clues} =
  if matches clue
    then card {clues = increment clue clues}
    else card
  where
    increment (RankClue _) Nothing      = Just Rank
    increment (RankClue _) (Just Color) = Just Both
    increment (RankClue _) x            = x
    increment (ColorClue _) Nothing     = Just Color
    increment (ColorClue _) (Just Rank) = Just Both
    increment (ColorClue _) x           = x
    matches (RankClue rank')   = rank' == rank
    matches (ColorClue color') = color' == color

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
