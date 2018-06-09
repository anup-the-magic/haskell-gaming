module Utils where

import           Data.Aeson       ((.=))
import qualified Data.Aeson       as Json
import qualified Data.Aeson.Types as Json.Types
import           Data.Text
import           Snap.Core        (Snap, modifyResponse, setResponseCode,
                                   writeLBS)

json :: [Json.Types.Pair] -> Snap ()
json = writeLBS . Json.encode . Json.object

errorWith :: Int -> Text -> Snap ()
errorWith code msg = do
  modifyResponse (setResponseCode code)
  json ["code" .= code, "msg" .= msg]
