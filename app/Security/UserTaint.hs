module Security.UserTaint (Taint, fmap, pure, (<*>), (>>=), getFromInt, getFromConfig) where

import Data.Configurator
import Data.Configurator.Types
import Data.Text
import System.IO.Unsafe
import Control.Exception

import Security.Taint

getFromConfig :: Taint a -> Maybe a
getFromConfig s = getFromInt s $ unsafePerformIO $ do cfg <- load [Required "app.cfg"]
                                                      require cfg $ pack "maxTaint"

getFromInt :: Taint a -> Int -> Maybe a
getFromInt s n =  if st > 0
                  then Just v
                  else Nothing
                  where (v, st) = app s n
