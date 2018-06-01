module Main where

import           Data.Foldable       (traverse_)
import qualified Data.List           as List
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.List.Split     as Split
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Hanabi
import qualified Hanabi.Parsers      as Parser
import           Hanabi.Types        (Err (..))
import qualified Hanabi.Types        as Types
import qualified System.Console.ANSI as ANSI
import qualified System.Exit         as Exit
import           System.IO           (stderr, stdout)
import qualified System.IO           as IO
import qualified System.Random       as Random

remainingLives :: Types.Lives -> Either Err Int
remainingLives lives =
  case lives of
    Types.Hissss -> return 3
    Types.Hisss  -> return 2
    Types.Hiss   -> return 1
    Types.Boom   -> Left BrokenState

remainingTurns :: Types.DeckState -> NonEmpty Types.Player -> Int
remainingTurns deck players =
  case deck of
    Types.Drawing cards -> NonEmpty.length cards + NonEmpty.length players
    Types.Ticking turns -> NonEmpty.length turns
    Types.NoCardsLeft   -> 0

getScore :: Map Types.Color (NonEmpty Types.Rank) -> Int
getScore = length . concatMap NonEmpty.toList . Map.elems

printWin :: Types.GameState -> IO a
printWin state = do
  cls
  putStrLn "You won!"
  putStrLn $ "Turns remaining: " ++ show (remainingTurns deck players)
  lives' <- either err return $ remainingLives lives
  putStrLn $ "Lives remaining: " ++ show lives'
  putStrLn $ "Score: " ++ show (getScore playables)
  Exit.exitSuccess
  where
    Types.GameState { Types.players = players
                    , Types.deck = deck
                    , Types.lives = lives
                    , Types.playables = playables
                    } = state

printLoss :: Types.GameState -> IO a
printLoss state = do
  cls
  putStrLn "You lost!"
  putStrLn $ "Score: " ++ show (getScore playables)
  Exit.exitSuccess
  where
    Types.GameState {Types.playables = playables} = state

printCurrentPlayer :: Types.GameState -> IO ()
printCurrentPlayer state = do
  putStrLn $ "Current player: " ++ show p
  putStrLn "Press enter to continue..."
  _ <- getLine
  return ()
  where
    Types.GameState {Types.players = (player :| _)} = state
    Types.PlayerId p = Types.playerId player

getPlayables :: Map Types.Color (NonEmpty Types.Rank) -> [String]
getPlayables = map (\(c, r :| _) -> [colorify c, rankify r]) . Map.assocs

colorify :: Types.Color -> Char
colorify Types.Red    = 'R'
colorify Types.Green  = 'G'
colorify Types.White  = 'W'
colorify Types.Yellow = 'Y'
colorify Types.Blue   = 'B'

rankify :: Types.Rank -> Char
rankify Types.One   = '1'
rankify Types.Two   = '2'
rankify Types.Three = '3'
rankify Types.Four  = '4'
rankify Types.Five  = '5'

printBoard :: Types.GameState -> IO ()
printBoard state = do
  lives' <- either err return $ remainingLives lives
  putStrLn $ "Lives Remaining: " ++ show lives'
  putStrLn $ "Turns Remaining: " ++ show (remainingTurns deck (p :| players))
  putStrLn $ "Cards Played: " ++ List.intercalate ", " (getPlayables playables)
  putStrLn "Cards Discarded: "
  let printCards :: [Types.Card] -> IO ()
      printCards = putStrLn . List.intercalate "," . map viewCardBasic
  traverse_ printCards $ Split.chunksOf 5 discards
  putStrLn $ "Clues: " ++ maybe "None!" show clues
  putStrLn $ "Your hand:" ++ hideHand p
  traverse_ (putStrLn . printHand) players
  where
    viewCardBasic :: Types.Card -> String
    viewCardBasic c = [colorify $ Types.color c, rankify $ Types.rank c]
    Types.GameState { Types.players = (p :| players)
                    , Types.lives = lives
                    , Types.playables = playables
                    , Types.clueTokens = clues
                    , Types.deck = deck
                    , Types.discards = discards
                    } = state
    hideHand player = List.intercalate "," . zipWith viewCard [1 ..] $ hand
      where
        hand = Types.hand player
        viewCard :: Int -> Types.Card -> String
        viewCard i Types.Card { Types.rank = rank
                              , Types.color = color
                              , Types.clues = clued
                              } = "(" ++ show i ++ ":" ++ card ++ ")"
          where
            card =
              case clued of
                Just Types.Both  -> [colorify color, rankify rank]
                Just Types.Color -> [colorify color, '_']
                Just Types.Rank  -> ['_', rankify rank]
                Nothing          -> ['_', '_']
    printHand player = pid ++ ": " ++ hand
      where
        (Types.PlayerId pid) = Types.playerId player
        hand = List.intercalate "," . map card . Types.hand $ player
        card Types.Card { Types.rank = rank
                        , Types.color = color
                        , Types.clues = clued
                        } = "(  " ++ text ++ ")"
          where
            on = '['
            off = ']'
            c = colorify color
            r = rankify rank
            text =
              case clued of
                Just Types.Both  -> [on, c, r, off]
                Just Types.Color -> [on, c, off, r]
                Just Types.Rank  -> [c, on, r, off]
                Nothing          -> [c, r]

getMove :: NonEmpty Types.Player -> IO Types.Move
getMove players = do
  inp <- getLine
  case Parser.move players inp of
    Left msg -> do
      print msg
      getMove players
    Right move -> return move

doMove :: Types.GameState -> IO ()
doMove state = do
  move <- getMove players
  let outcome = Hanabi.performMove move state
  case outcome of
    Right (Types.Win state')      -> printWin state'
    Right (Types.Loss state')     -> printLoss state'
    Right (Types.Continue state') -> runGame state'
    Left msg                      -> print msg >> doMove state
  where
    Types.GameState {Types.players = players} = state

runGame :: Types.GameState -> IO ()
runGame state = do
  cls
  printCurrentPlayer state
  printBoard state
  doMove state

main :: IO ()
main = do
  IO.hSetBuffering stdout IO.NoBuffering
  IO.hSetBuffering stderr IO.NoBuffering
  putStrLn "Hello"
  gen <- Random.newStdGen
  state <- createGame gen
  runGame state
  where
    createGame gen = do
      putStrLn "How many players?"
      inp <- getLine
      let n = read inp
      case Hanabi.init n gen of
        Left msg    -> print msg >> createGame gen
        Right state -> return state

err :: (Show a) => a -> IO b
err msg = do
  IO.hPrint stderr msg
  Exit.exitWith (Exit.ExitFailure 1)

cls :: IO ()
cls = do
  ANSI.setSGR [ANSI.Reset]
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
