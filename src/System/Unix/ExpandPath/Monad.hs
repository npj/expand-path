module System.Unix.ExpandPath.Monad (
    ExpandPathM
  , homeDir
  , userHomeDir
  , currentDir
  , expandPathM
  ) where


import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (FilePath)
import System.Directory (getHomeDirectory, getCurrentDirectory)
import System.FilePath.Posix ((</>), normalise)
import System.Posix.User (UserEntry (homeDirectory), getUserEntryForName)

class (Monad m) => ExpandPathM m where
  homeDir     :: m FilePath
  userHomeDir :: String -> m FilePath
  currentDir  :: m FilePath

instance ExpandPathM IO where
  homeDir = getHomeDirectory
  userHomeDir name = homeDirectory <$> getUserEntryForName name
  currentDir = getCurrentDirectory

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

expandPathM :: (ExpandPathM m) => FilePath -> m FilePath
expandPathM origPath = case trim (normalise origPath) of
  []           -> pure []
  ('~':rest)   -> expandHome rest
  path@('/':_) -> pure path
  path@_       -> (</>) <$> currentDir <*> pure path

  where expandHome path = case break (== '/') path of
          ("", rest)   -> (</>) <$> homeDir          <*> pure (deslash rest)
          (name, rest) -> (</>) <$> userHomeDir name <*> pure (deslash rest)

        deslash path = case path of
          []         -> []
          ('/':rest) -> rest
          _          -> path
