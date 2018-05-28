{-# LANGUAGE NamedFieldPuns #-}

module Hanabi where

import           Control.Monad         (mfilter)
import qualified Data.List             as List
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.List.Split       as Split
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Hanabi.Types          (Card, CardId (..), ClueTokens (..),
                                        ClueType (..), Color, DeckState (..),
                                        Err (..), GameState, Lives (..),
                                        Move (..), MoveOutcome (..), Player,
                                        PlayerId (..), Rank)
import qualified Hanabi.Types          as Types
import           System.Random         (RandomGen)
import qualified System.Random.Shuffle as Shuffle

init :: (RandomGen gen) => Int -> gen -> Either Err GameState
init nPlayers gen = do
  (players, deck') <- startingHands
  return
    Types.GameState
    { Types.players = players
    , Types.deck = Types.Drawing deck'
    , Types.playables = Map.empty
    , Types.lives = Hissss
    , Types.clueTokens = Just Clue8
    , Types.discards = []
    }
  where
    cards =
      [(r, c) | r <- [Types.One .. Types.Five], c <- [Types.Red .. Types.White]]
    deck =
      zipWith createCard [1 ..] . Shuffle.shuffle' cards (length cards) $ gen
    createCard :: Int -> (Rank, Color) -> Card
    createCard i (r, c) =
      Types.Card
      { Types.rank = r
      , Types.color = c
      , Types.cardId =
          Types.CardId (show i ++ " - " ++ Types.colorStr c ++ Types.rankStr r)
      , Types.clues = Nothing
      }
    handSize 2 = Right 5
    handSize 3 = Right 5
    handSize 4 = Right 4
    handSize 5 = Right 4
    handSize x = Left (InvalidPlayerCount x)
    toPlayer :: Char -> [Card] -> Player
    toPlayer char hand =
      Types.Player {Types.playerId = Types.PlayerId [char], Types.hand = hand}
    startingHands = do
      hand <- handSize nPlayers
      let (hands, deck') = List.splitAt (hand * nPlayers) deck
      let players = zipWith toPlayer ['A' ..] . Split.chunksOf hand $ hands
      return (NonEmpty.fromList players, NonEmpty.fromList deck')

performMove :: Move -> GameState -> Either Err MoveOutcome
performMove move state = tick <$> performMove' move state

performMove' :: Move -> GameState -> Either Err GameState
performMove' (Play cardId) state = playCard cardId state
performMove' (Clue clue target cards) state =
  validate clue target cards (Types.players state) >>= performClue state
performMove' (Discard cardId) state = discardCard cardId state

tick :: GameState -> MoveOutcome
tick state@Types.GameState {Types.players} =
  checkOutcome $ state {Types.players = rotateNEL players}

data ClueState = ClueState
  { players :: (NonEmpty Player, Player, [Player])
  , clue    :: ClueType
  }

validate ::
     ClueType
  -> PlayerId
  -> Set CardId
  -> NonEmpty Player
  -> Either Err ClueState
validate clue playerId targetCards players = do
  (before, player, after) <-
    maybe (Left (PlayerNotFound playerId)) Right (getPlayer players)
  if Set.null . Set.difference targetCards . clueCards $ player
    then return ClueState {players = (before, player, after), clue}
    else Left (ClueNotComplete clue playerId targetCards)
  where
    clueCards =
      Set.fromList . map Types.cardId . filter (clueCards' clue) . Types.hand
    clueCards' (RankClue rank)   = (rank ==) . Types.rank
    clueCards' (ColorClue color) = (color ==) . Types.color
    zippers :: [a] -> [([a], a, [a])]
    zippers xs = zip3 (List.inits xs) xs (drop 1 . List.tails $ xs)
    getPlayer (curr :| xs) =
      (\(pre, p, post) -> (curr :| pre, p, post)) <$>
      List.find
        (\(_, p, _) -> ((playerId ==) . Types.playerId $ p))
        (zippers xs)

performClue :: GameState -> ClueState -> Either Err GameState
performClue state clueState = do
  clueTokens <- tokens
  return state {Types.clueTokens, Types.players}
  where
    tokens = decrementToken <$> tokens'
    tokens' = maybe (Left NoCluesLeft) Right $ Types.clueTokens state
    decrementToken Clue1 = Nothing
    decrementToken x     = Just . pred $ x
    ClueState {players = (curr :| playersBefore, player, playersAfter), clue} =
      clueState
    player' =
      player {Types.hand = map (Types.clueCard clue) . Types.hand $ player}
    players = curr :| (playersBefore ++ (player' : playersAfter))

discardCard :: CardId -> GameState -> Either Err GameState
discardCard cardId state =
  discardCard' state =<< getCardFromCurrentPlayer cardId (Types.players state)

playCard :: CardId -> GameState -> Either Err GameState
playCard cardId state =
  playCard' state =<< getCardFromCurrentPlayer cardId (Types.players state)

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
  (players', deck') <- drawCard deck cardState
  (playables', lives') <- tryPlayCard playables lives currentCard
  return $
    gameState
    { Types.players = players'
    , Types.deck = deck'
    , Types.playables = playables'
    , Types.lives = lives'
    }
  where
    Types.GameState {Types.deck, Types.playables, Types.lives} = gameState
    CardState {currentCard} = cardState

discardCard' :: GameState -> CardState -> Either Err GameState
discardCard' gameState cardState = do
  (players', deck') <- drawCard deck cardState
  return
    gameState
    { Types.discards = currentCard : discards
    , Types.deck = deck'
    , Types.players = players'
    , Types.clueTokens = increment clueTokens
    }
  where
    Types.GameState {Types.deck, Types.discards, Types.clueTokens} = gameState
    CardState {currentCard} = cardState
    increment Nothing      = Just Clue1
    increment (Just Clue8) = Just Clue8
    increment xs           = succ <$> xs

-- possible rewrite using Map.alterF :: State Lives?
tryPlayCard ::
     Map Color [Rank] -> Lives -> Card -> Either Err (Map Color [Rank], Lives)
tryPlayCard playables lives Types.Card {Types.rank = Types.One, Types.color} =
  case played of
    Nothing -> Right (Map.insert color [Types.One] playables, lives)
    Just _  -> (,) <$> Right playables <*> decrementLives lives
  where
    played = Map.lookup color playables
tryPlayCard playables lives Types.Card {Types.rank, Types.color} =
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

drawCard :: DeckState -> CardState -> Either Err (NonEmpty Player, DeckState)
drawCard (Drawing (card :| rest)) CardState { restOfHand
                                            , currentPlayer
                                            , restOfPlayers
                                            } = Right (players, deck)
  where
    player = currentPlayer {Types.hand = card : restOfHand}
    players = player :| restOfPlayers
    deck = maybe (Ticking players) Drawing (NonEmpty.nonEmpty rest)
drawCard (Ticking (_ :| rest)) CardState {currentPlayer, restOfPlayers} =
  Right (players, deck)
  where
    players = currentPlayer :| restOfPlayers
    deck = maybe NoCardsLeft Ticking (NonEmpty.nonEmpty rest)
drawCard NoCardsLeft _ = Left BrokenState

rotateNEL :: NonEmpty a -> NonEmpty a
rotateNEL (x :| xs) = NonEmpty.fromList (xs ++ [x])

checkOutcome :: GameState -> MoveOutcome
checkOutcome state@Types.GameState {Types.lives = Boom}       = Loss state
checkOutcome state@Types.GameState {Types.deck = NoCardsLeft} = Win state
checkOutcome state                                            = Continue state
