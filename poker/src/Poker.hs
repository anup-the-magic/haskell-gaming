{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Poker where

import qualified Deck
import Deck (Card, Deck)
import Poker.Types

getId :: Int
getId = 1

initialize :: Int -> Int -> Game
initialize n buyIn =
  Game
  { _id = getId
  , deck = Deck.empty
  , players = map (\x -> Player {_id = x, hand = [], stack = buyIn}) [0 .. n]
  , board = Nothing
  }

-- TODO
decideWinner :: Game -> Game
decideWinner = id

advanceState' :: Game -> (Pot, [Player]) -> Game
advanceState' game@Game {board = Nothing} (pot, players) =
  game {players = players, board = Just $ PreFlop pot}
advanceState' game@Game {board = Just (PreFlop _), deck = c1:c2:c3:deck} (pot, players) =
  game {players = players, board = Just $ Flop pot c1 c2 c3, deck = deck}
advanceState' game@Game {board = Just (Flop _ c1 c2 c3), deck = c4:deck} (pot, players) =
  game {players = players, board = Just $ Turn pot c1 c2 c3 c4, deck = deck}
advanceState' game@Game {board = Just (Turn _ c1 c2 c3 c4), deck = c5:deck} (pot, players) =
  game {players = players, board = Just $ River pot c1 c2 c3 c4 c5, deck = deck}
advanceState' game@Game {board = Just (River _ c1 c2 c3 c4 c5)} (pot, players) =
  game {board = Just $ River pot c1 c2 c3 c4 c5, players = players}

-- TODO
betting :: Pot -> [Player] -> [Card] -> (Pot, [Player])
betting pot ps cs = (pot, ps)

getPot :: Game -> Pot
getPot Game {board = Nothing} = 0
getPot Game {board = Just (PreFlop p)} = p
getPot Game {board = Just (Flop p _ _ _)} = p
getPot Game {board = Just (Turn p _ _ _ _)} = p
getPot Game {board = Just (River p _ _ _ _ _)} = p

getCards :: Game -> [Card]
getCards Game {board = Nothing} = []
getCards Game {board = Just (PreFlop _)} = []
getCards Game {board = Just (Flop _ c1 c2 c3)} = [c1, c2, c3]
getCards Game {board = Just (Turn _ c1 c2 c3 c4)} = [c1, c2, c3, c4]
getCards Game {board = Just (River _ c1 c2 c3 c4 c5)} = [c1, c2, c3, c4, c5]

advanceState :: Game -> Game
advanceState game = advanceState' game $ betting (getPot game) (players game) (getCards game)

-- TODO
playHand :: Game -> Game
playHand = decideWinner . advanceState . advanceState . advanceState

removeLosers :: Game -> Game
removeLosers game@Game {players = ps} = game {players = filter ((<= 0) . stack) ps}

checkWinners :: Game -> Either (Maybe Player) Game
checkWinners Game {players = [p]} = Left . Just $ p
checkWinners Game {players = []} = Left Nothing
checkWinners game = Right game

play :: Game -> Maybe Player
play = either id play . checkWinners . removeLosers . playHand
