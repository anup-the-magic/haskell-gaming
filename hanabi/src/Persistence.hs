{-# LANGUAGE DuplicateRecordFields #-}

-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE TypeFamilies          #-}
module Persistence where

import Control
import           Database.Beam
import           Database.Beam.Postgres
import           Persistence.Types

getGame :: Connection -> GameId -> IO (Maybe Game)
getGame connection =
  runBeamPostgres connection . runSelectReturningOne . lookup_ (games db)

getGamePlayers :: Connection -> GameId -> IO [GamePlayer]
getGamePlayers connection gid =
  runBeamPostgres connection . runSelectReturningList . select $ do
    filter_ (\gp -> gpGid gp ==. gid) (all_ (game_players db))
  where
    gpGid = game_id :: GamePlayerT f -> PrimaryKey GameT f
