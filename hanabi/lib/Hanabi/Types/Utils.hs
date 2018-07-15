module Hanabi.Types.Utils where

import           Hanabi.Types (Card (..), Clue (..), ClueType (..), Color (..),
                               Rank (..))

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

clueCard :: ClueType -> Card -> Card
clueCard clue card@Card {color = color, rank = rank, clues = clues} =
  if matches clue
    then card {clues = increment clue clues}
    else card
  where
    matches (RankClue rank')   = rank' == rank
    matches (ColorClue color') = color' == color
    increment (RankClue _) Nothing      = Just Rank
    increment (RankClue _) (Just Color) = Just Both
    increment (RankClue _) x            = x
    increment (ColorClue _) Nothing     = Just Color
    increment (ColorClue _) (Just Rank) = Just Both
    increment (ColorClue _) x           = x
