module System.Unix.ExpandPath (
    expandPath
  ) where

import System.Unix.ExpandPath.Monad (expandPathM)

expandPath :: FilePath -> IO FilePath
expandPath = expandPathM
