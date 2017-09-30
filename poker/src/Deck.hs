module Deck where

data Suit
  = Club
  | Diamond
  | Heart
  | Spade
  deriving (Read, Show, Enum, Eq, Ord)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Read, Show, Enum, Eq, Ord)

data Card = Card
  { suit :: Suit
  , rank :: Rank
  } deriving (Read, Show, Eq)

instance Ord Card where
  compare c1 c2 = compare (rank c1, suit c1) (rank c2, suit c2)

instance Enum Card where
  fromEnum c = 4 * (fromEnum . rank) c + (fromEnum . suit) c
  toEnum n =
    let (rank', suit') = divMod n 4
    in Card {suit = toEnum suit', rank = toEnum rank'}

type Deck = [Card]

empty :: Deck
empty = [Card {suit = s, rank = r} | s <- [Club .. Diamond], r <- [Two .. Ace]]
