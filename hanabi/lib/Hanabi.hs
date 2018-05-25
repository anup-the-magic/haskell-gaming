{-# LANGUAGE NamedFieldPuns #-}

module Hanabi where

import           Control.Monad      (mfilter)
import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified Hanabi.Deck        as Deck
import           Hanabi.Types       (Card, CardId (..), DeckState (..),
                                     Err (..), GameState, Lives (..), Move (..),
                                     MoveOutcome (..), Player)
import qualified Hanabi.Types       as Types

performMove :: Move -> GameState -> Either Err MoveOutcome
performMove (Play cardId) = playCard cardId
performMove _             = undefined

playCard :: CardId -> GameState -> Either Err MoveOutcome
playCard cardId state =
  checkOutcome <$>
  (playCard' state =<< getCardFromCurrentPlayer cardId (Types.players state))

getCardFromCurrentPlayer :: CardId -> NonEmpty Player -> Either Err CardState
getCardFromCurrentPlayer cardId (player :| players) =
  case splitHand of
    ([x], xs) -> Right $ CardState x xs player players
    _         -> Left CardNotOwned
  where
    hand = Types.hand player
    splitHand = List.partition ((== cardId) . Types.cardId) hand

data CardState = CardState
  { currentCard   :: Card
  , restOfHand    :: [Card]
  , currentPlayer :: Player
  , restOfPlayers :: [Player]
  }

playCard' :: GameState -> CardState -> Either Err GameState
playCard' gameState cardState = do
  (players', deck') <- drawCardAndRotate deck cardState
  (playables', lives') <- tryPlayCard playables lives (Types.card currentCard)
  return $ Types.GameState players' deck' playables' lives'
  where
    Types.GameState {Types.deck, Types.playables, Types.lives} = gameState
    CardState {currentCard} = cardState

-- possible rewrite using Map.alterF :: State Lives?
tryPlayCard ::
     Map Deck.Color [Deck.Rank]
  -> Lives
  -> Deck.Card
  -> Either Err (Map Deck.Color [Deck.Rank], Lives)
tryPlayCard playables lives Deck.Card {Deck.rank = Deck.One, Deck.color} =
  case played of
    Nothing -> Right (Map.insert color [Deck.One] playables, lives)
    Just _  -> (,) <$> Right playables <*> decrementLives lives
  where
    played = Map.lookup color playables
tryPlayCard playables lives Deck.Card {Deck.rank, Deck.color} =
  case played of
    Just xs -> Right (Map.insert color (rank : xs) playables, lives)
    Nothing -> (,) <$> Right playables <*> decrementLives lives
  where
    played = mfilter (rankOneLess rank) . Map.lookup color $ playables
    rankOneLess rankA (rankB:_) = rankB == pred rankA
    rankOneLess _ _             = False -- TODO collapse cases

decrementLives :: Lives -> Either Err Lives
decrementLives Hissss = Right Hisss
decrementLives Hisss  = Right Hiss
decrementLives Hiss   = Right Boom
decrementLives Boom   = Left BrokenState

drawCardAndRotate ::
     DeckState -> CardState -> Either Err (NonEmpty Player, DeckState)
drawCardAndRotate (Drawing (card :| rest)) CardState { restOfHand
                                                     , currentPlayer
                                                     , restOfPlayers
                                                     } = Right (players, deck)
  where
    player = currentPlayer {Types.hand = card : restOfHand}
    players = appendToCreateNEL restOfPlayers player
    deck = maybe (Ticking players) Drawing (NonEmpty.nonEmpty rest)
drawCardAndRotate (Ticking (_ :| rest)) CardState {currentPlayer, restOfPlayers} =
  Right (players, deck)
  where
    players = appendToCreateNEL restOfPlayers currentPlayer
    deck = maybe NoCardsLeft Ticking (NonEmpty.nonEmpty rest)
drawCardAndRotate NoCardsLeft _ = Left BrokenState

-- Note, NonEmpty.fromList throws, but since we append we can ignore the throw
appendToCreateNEL :: [a] -> a -> NonEmpty a
appendToCreateNEL xs x = NonEmpty.fromList (xs ++ [x])

checkOutcome :: GameState -> MoveOutcome
checkOutcome state@Types.GameState {Types.lives = Boom}       = Loss state
checkOutcome state@Types.GameState {Types.deck = NoCardsLeft} = Win state
checkOutcome state                                            = Continue state
