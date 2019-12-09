module Routing.API where

data CreateGame = CreateGame {}
data GameState = GameState {}

data API = API
         { newGame :: CreateGame -> GameState
         , health :: () -> String
         }
