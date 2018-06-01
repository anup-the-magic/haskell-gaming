{-# LANGUAGE ViewPatterns #-}

module Hanabi.Parsers where

import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.Split    as Split
import qualified Hanabi
import           Hanabi.Types       (Card, ClueType (..), Color (..), Move (..),
                                     PlayerId, Rank (..))
import qualified Hanabi.Types       as Types
import qualified Text.Read          as Read

data ParserError
  = CardNotFound (Rank, Color)
  | PlayerNotFound PlayerId
  | InvalidClueInput String
  | InvalidRankInput Char
  | InvalidColorInput Char
  | InvalidRankColorInput String
  | InvalidMoveInput String
  | InvalidIndex Char
  deriving (Eq, Show)

move :: NonEmpty Types.Player -> String -> Either ParserError Move
move (p :| _) (words -> ["play", [i]]) = do
  input <-
    maybe (Left $ InvalidIndex i) return (Read.readMaybe [i] :: Maybe Int)
  card <-
    maybe (Left $ InvalidIndex i) return $
    List.lookup input . zip [1 ..] $ Types.hand p
  return $ Play (Types.cardId card)
move (p :| _) (words -> ["discard", [i]]) = do
  input <-
    maybe (Left $ InvalidIndex i) return (Read.readMaybe [i] :: Maybe Int)
  card <-
    maybe (Left $ InvalidIndex i) return $
    List.lookup input . zip [1 ..] $ Types.hand p
  return $ Discard (Types.cardId card)
move (_ :| players) (words -> ["clue", p, clueInp]) = do
  player <-
    maybe (Left $ PlayerNotFound (Types.PlayerId p)) return $
    List.find (\x -> Types.playerId x == Types.PlayerId p) players
  c <- clue clueInp
  let cards = Hanabi.clueCards c player
  return $ Clue c (Types.PlayerId p) cards
move _ inp = Left $ InvalidMoveInput inp

findCard :: [Card] -> String -> Either ParserError Card
findCard hand cardInp = do
  (r, c) <- rankColor cardInp
  clued r c hand
  where
    clued r c =
      maybe (Left $ CardNotFound (r, c)) return .
      List.find (\x -> Types.rank x == r && Types.color x == c)

clue :: String -> Either ParserError ClueType
clue (Split.splitOn ":" -> ["color", [c]]) = do
  x <- color c
  return $ ColorClue x
clue (Split.splitOn ":" -> ["rank", [r]]) = do
  x <- rank r
  return $ RankClue x
clue xs = Left $ InvalidClueInput xs

rankColor :: String -> Either ParserError (Rank, Color)
rankColor [c, r] = (,) <$> rank r <*> color c
rankColor xs     = Left $ InvalidRankColorInput xs

color :: Char -> Either ParserError Color
color 'R' = return Red
color 'G' = return Green
color 'W' = return White
color 'B' = return Blue
color 'Y' = return Yellow
color x   = Left $ InvalidColorInput x

rank :: Char -> Either ParserError Rank
rank '1' = return One
rank '2' = return Two
rank '3' = return Three
rank '4' = return Four
rank '5' = return Five
rank x   = Left $ InvalidRankInput x
