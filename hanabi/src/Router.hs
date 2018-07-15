module Router where

import           Control.Applicative ((<|>))
import           Snap.Core           (Snap, route, writeBS)
import           Snap.Utils          (errorWith)

routes :: Snap ()
routes = route handlers <|> errorWith 404 "Route not found"
  where
    handlers = [("hanabi", writeBS "hanabs"), ("health", writeBS "Healthy")]
