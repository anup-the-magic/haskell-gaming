module Utils where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Json
import           Data.Text
import           Snap.Core  (Snap, modifyResponse, setResponseCode, writeLBS)

json :: Json.Value -> Snap ()
json = writeLBS . Json.encode

errorWith :: Int -> Text -> Snap ()
errorWith code msg = do
  modifyResponse (setResponseCode code)
  json (Json.object ["code" .= code, "msg" .= msg])
