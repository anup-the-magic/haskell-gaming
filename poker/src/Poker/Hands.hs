{-# LANGUAGE DuplicateRecordFields #-}

module Poker.Hands where

import Deck
import Poker.Types

data Kickers = Kickers Card Card Card Card Card

data Hand
  = StraightFlush { rank :: Rank
                 ,  suit :: Suit
                 ,  kickers :: Kickers}
  | FourOfAKind { rank :: Rank
               ,  kickers :: Kickers}
  | FullHouse { threes :: Rank, pair :: Rank, kickers :: Kickers }
  | Flush { kickers :: Kickers, suit :: Suit }
  | Straight { kickers :: Kickers, low :: Rank }
