{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (id)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath (joinPath)
import System.Environment
import Data.Time

data Entry
  = Entry
  { id :: Int
  , task :: String
  , inTime :: UTCTime
  , outTime :: Maybe UTCTime
  }

instance FromRow Entry where
  fromRow = Entry <$>  field <*>  field <*>  field <*> field

data Minutes
  = Minutes
  { name :: String
  , minutes :: Float
  }

instance Show Minutes where
  show (Minutes { name, minutes }) = name <> ": " <> show (round minutes) <> " mins"

instance FromRow Minutes where
  fromRow = Minutes <$> field <*> field

expandHome :: String -> IO FilePath
expandHome p = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, p])

-- 'punch in project_name'
-- punch out
-- punch list project_name
main :: IO ()
main = do
  (cmd:rest) <- getArgs
  path <- expandHome "punch.db"
  case cmd of
    "in" -> do
      putStrLn "Going in"
      conn <- open path
      rows <- query_ conn rowCheck :: IO [Entry]
      if length rows > 0
        then putStrLn "Can't punch in again"
        else
          case headMaybe rest of
            Just task -> do
              p $ "Punching into " <> task
              execute conn punchIn (Only task)
            Nothing -> putStrLn "Must provide a task name to check in"
      return ()
    "out" -> do
      conn <- open path
      rows <- query_ conn rowCheck :: IO [Entry]
      case headMaybe rows of
        Just (Entry { id, task }) -> do
          p $ "Punching out from " <> task
          execute conn punchOut (Only id)
        Nothing -> p "Can't punch out if you're not in..."
    "list" -> do
      conn <- open path
      rows <- query_ conn minutesQuery :: IO [Minutes]
      mapM_ print rows
p = putStrLn

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

schema :: Query
schema = "create table tasks( \
  \ id integer primary key, \
  \ task text, \
  \ intime utctime not null, \
  \ outtime utctime);"

rowCheck = "select * from tasks where outtime is null order by intime desc limit 1"
punchIn = "insert into tasks(task, intime, outtime) values(?, datetime(), null)"
punchOut = "update tasks set outtime = datetime() where id = ?"
minutesQuery = "select task, (julianday(outtime) - julianday(intime))*1440.0 from tasks"
