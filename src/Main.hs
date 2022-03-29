{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleInstances, NamedFieldPuns, OverloadedStrings, StandaloneDeriving, TypeOperators #-}

module Main where

import Data.Functor
import Data.Maybe
import Data.Time
import Database.SQLite.Simple
import Options.Generic
import Prelude hiding (id, init)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath (joinPath)

data Options
  = In String
  | Out
  | List { name :: Maybe String, days :: Maybe Int }
  | Total { name :: Maybe String, days :: Maybe Int}
  deriving (Show, Generic, ParseRecord)

-- instance ParseRecord (Options Wrapped)
-- instance ParseRecord (Options Unwrapped)
-- deriving instance Show (Options Unwrapped)

data Entry
  = Entry
  { id :: Int
  , task :: String
  , inTime :: UTCTime
  , outTime :: Maybe UTCTime
  , billed :: Bool
  }

instance FromRow Entry where
  fromRow = Entry <$>  field <*>  field <*>  field <*> field <*> field

data Minutes
  = Minutes
  { _name :: String
  , _intime :: UTCTime
  , _minutes :: Float
  }

instance Show Minutes where
  show (Minutes { _name, _minutes, _intime }) =
    _name
    <> " ( " <> displayDay _intime <> " ): "
    <> show (round _minutes)
    <> " mins"

displayDay :: UTCTime -> String
displayDay = formatTime defaultTimeLocale "%D"

instance FromRow Minutes where
  fromRow = Minutes <$> field <*> field <*> field

init path = do
  p $ "Initializing punch db @ " <> path
  conn <- open path
  execute_ conn schema

expandHome :: String -> IO FilePath
expandHome pth = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, pth])

main :: IO ()
main = do
  opts <- getRecord "Punch"
  path <- expandHome "punch.db"
  -- (cmd:rest) <- getArgs
  exists <- doesFileExist path
  if exists then mempty else init path
  case opts of
    In task -> do
      conn <- open path
      rows <- query_ conn rowCheck :: IO [Entry]
      if length rows > 0
        then putStrLn "Can't punch in again"
        else do
          p $ "Punching into " <> task
          execute conn punchIn (Only task)
      return ()
    Out -> do
      conn <- open path
      rows <- query_ conn rowCheck :: IO [Entry]
      case headMaybe rows of
        Just (Entry { id, task }) -> do
          p $ "Punching out from " <> task
          execute conn punchOut (Only id)
        Nothing -> p "Can't punch out if you're not in..."
    List { name = mtask, days = days'}-> do
      -- let days = fromMaybe 7 days'
      conn <- open path
      rows <- case mtask of
        Just task ->
          query conn minutesQueryWithTask (Only task) :: IO [Minutes]
        Nothing ->
          query_ conn $ minutesQuery :: IO [Minutes]
      mapM_ print rows
    Total { name = task, days = days'} -> do
      -- let days = fromMaybe 7 days'
      conn <- open path
      rows <- query conn minutesQueryWithTask (Only task) :: IO [Minutes]
      let total = sum $ map (\Minutes {_minutes} -> round _minutes :: Integer) rows
      p $ "total: " <> show (total `div` 60) <> " hours"

p = putStrLn

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

schema :: Query
schema = "create table tasks( \
  \ id integer primary key, \
  \ task text, \
  \ intime utctime not null, \
  \ outtime utctime, \
  \ billed boolean);"

rowCheck = "select * from tasks where outtime is null order by intime desc limit 1"
punchIn = "insert into tasks(task, intime, outtime, billed) values(?, datetime(), null, false)"
punchOut = "update tasks set outtime = datetime() where id = ?"
minutesQuery = "select task, intime, (julianday(outtime) - julianday(intime))*1440.0 \
  \ from tasks where outtime is not null \
  \ and billed is false"
minutesQueryWithTask = "select task, \
  \ intime, \
  \ (julianday(outtime) - julianday(intime))*1440.0 \
  \ from tasks where task = ? \
  \ and outtime is not null \
  \ and billed is false"
