module Hanabi.Deck where

import           System.Random         (RandomGen)
import           System.Random.Shuffle (shuffle')

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
  { color :: Color
  , rank  :: Rank
  } deriving (Eq, Show)

deck :: (RandomGen r) => r -> [Card]
deck = shuffle' cards (length cards)
  where
    cards =
      [ Card c r
      | c <- [Red .. White]
      , r <- [One, One, One, Two, Two, Three, Three, Three, Four, Four, Five]
      ]
