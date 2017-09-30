{-# LANGUAGE DuplicateRecordFields #-}

module Poker.Types where

import Deck (Card, Deck)

data Player = Player
  { _id :: Int
  , hand :: [Card]
  , stack :: Int
  }

type Pot = Int

data Board
  = PreFlop Pot
  | Flop Pot
         Card
         Card
         Card
  | Turn Pot
         Card
         Card
         Card
         Card
  | River Pot
          Card
          Card
          Card
          Card
          Card

data Game = Game
  { _id :: Int
  , deck :: Deck
  , players :: [Player]
  , board :: Maybe Board
  }
