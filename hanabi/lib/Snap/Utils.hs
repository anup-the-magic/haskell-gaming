module Snap.Utils where

import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Json
import           Data.Text
import           Snap.Core  (Snap, modifyResponse, setResponseCode, writeLBS)

json :: ToJSON a => a -> Snap ()
json = writeLBS . Json.encode

errorWith :: Int -> Text -> Snap ()
errorWith code msg = do
  modifyResponse (setResponseCode code)
  json (Json.object ["code" .= code, "msg" .= msg])
