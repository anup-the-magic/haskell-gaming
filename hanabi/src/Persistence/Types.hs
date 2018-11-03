{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Persistence.Types where

import           Data.Text     (Text)
import           Database.Beam
import           GHC.Generics  (Generic)
import qualified Hanabi.Types  as Types
import           Prelude       hiding (id)

data PlayerT f = Player
  { player_id :: C f Text
  , name      :: C f Text
  } deriving (Generic)

instance Table PlayerT where
  data PrimaryKey PlayerT f = PlayerId (C f Text)
                              deriving Generic
  primaryKey = PlayerId . (player_id :: PlayerT f -> C f Text)

data GameT f = Game
  { game_id :: C f Text
  , seed    :: C f Int
  } deriving (Generic)

instance Table GameT where
  data PrimaryKey GameT f = GameId (C f Text)
                            deriving Generic
  primaryKey = GameId . (game_id :: GameT f -> C f Text)

data GamePlayerT f = GamePlayer
  { game_id   :: PrimaryKey GameT f
  , player_id :: PrimaryKey PlayerT f
  , order     :: C f Int
  } deriving (Generic)

instance Table GamePlayerT where
  data PrimaryKey GamePlayerT f = GPId (PrimaryKey GameT f)
                                     (PrimaryKey PlayerT f)
                                  deriving Generic
  primaryKey gp =
    GPId
      ((game_id :: GamePlayerT f -> PrimaryKey GameT f) gp)
      ((player_id :: GamePlayerT f -> PrimaryKey PlayerT f) gp)

data MoveInfoMixin f = MoveInfo
  { game_id   :: PrimaryKey GameT f
  , player_id :: PrimaryKey PlayerT f
  , index     :: C f Int
  } deriving (Generic)

data RankT f = Rank
  { rank_id :: C f Text
  , move    :: MoveInfoMixin f
  , cluing  :: PrimaryKey PlayerT f
  , rank    :: C f Types.Rank
  } deriving (Generic)

instance Table RankT where
  data PrimaryKey RankT f = RankId (C f Text)
                            deriving Generic
  primaryKey = RankId . (rank_id :: RankT f -> C f Text)

data ColorT f = Color
  { color_id :: C f Text
  , move     :: MoveInfoMixin f
  , cluing   :: C f Types.PlayerId
  , rank     :: C f Types.Color
  } deriving (Generic)

instance Table ColorT where
  data PrimaryKey ColorT f = ColorId (C f Text)
                             deriving Generic
  primaryKey = ColorId . (color_id :: ColorT f -> C f Text)

data PlayT f = Play
  { play_id :: C f Text
  , move    :: MoveInfoMixin f
  , card    :: C f Int
  } deriving (Generic)

instance Table PlayT where
  data PrimaryKey PlayT f = PlayId (C f Text)
                            deriving Generic
  primaryKey = PlayId . (play_id :: PlayT f -> C f Text)

data DiscardT f = Discard
  { discard_id :: C f Text
  , move       :: MoveInfoMixin f
  , card       :: C f Int
  } deriving (Generic)

instance Table DiscardT where
  data PrimaryKey DiscardT f = DiscardId (C f Text)
                               deriving Generic
  primaryKey = DiscardId . (discard_id :: DiscardT f -> C f Text)

data DB f = DB
  { players      :: f (TableEntity PlayerT)
  , games        :: f (TableEntity GameT)
  , game_players :: f (TableEntity GamePlayerT)
  , rank_clues   :: f (TableEntity RankT)
  , color_clues  :: f (TableEntity ColorT)
  , plays        :: f (TableEntity PlayT)
  , discards     :: f (TableEntity DiscardT)
  } deriving (Generic)

instance Database backend DB

db :: DatabaseSettings backend DB
db = defaultDbSettings

{------------------
- BEAM BOILERPLATE -
 ------------------}
-- Usable types
type Player = PlayerT Identity

deriving instance Show Player

type Game = GameT Identity

type GameId = PrimaryKey GameT Identity

deriving instance Show Game

type GamePlayer = GamePlayerT Identity

deriving instance Show (PrimaryKey GameT Identity)

deriving instance Show (PrimaryKey PlayerT Identity)

deriving instance Show GamePlayer

type MoveInfo = MoveInfoMixin Identity

deriving instance Show MoveInfo

type Rank = RankT Identity

deriving instance Show Rank

type Color = ColorT Identity

deriving instance Show Color

type Play = PlayT Identity

deriving instance Show Play

type Discard = DiscardT Identity

deriving instance Show Discard

-- Beam Tables
instance Beamable PlayerT

instance Beamable (PrimaryKey PlayerT)

instance Beamable GameT

instance Beamable (PrimaryKey GameT)

instance Beamable GamePlayerT

instance Beamable (PrimaryKey GamePlayerT)

instance Beamable MoveInfoMixin

instance Beamable RankT

instance Beamable (PrimaryKey RankT)

instance Beamable ColorT

instance Beamable (PrimaryKey ColorT)

instance Beamable PlayT

instance Beamable (PrimaryKey PlayT)

instance Beamable DiscardT

instance Beamable (PrimaryKey DiscardT)
