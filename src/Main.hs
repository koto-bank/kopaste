{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Concurrent

import System.Directory

import Control.Monad (msum)
import Control.Monad.Zip

import Data.Time.Clock
import Data.Time.Calendar

import Happstack.Server

import Pages

runCleanup :: IO ()
runCleanup = do
  pastePaths <- (map $ (++) "pastes/") <$> listDirectory "pastes"
  modificationTimes <- mapM getModificationTime pastePaths
  pathTimeList <- return $ zip pastePaths modificationTimes
  currTime <- getCurrentTime
  mapM_
    (\(path, tm) ->
       let UTCTime dayCurrent _ = currTime
           UTCTime dayPaste _ = tm
           pasteDiff = diffDays dayCurrent dayPaste
       in if pasteDiff >= 7 -- delete after a week
            then removeFile path
            else return ())
    pathTimeList
  threadDelay 3600000000 -- an hour
  runCleanup


kopasteApp :: ServerPart Response
kopasteApp =
  msum
    [ do nullDir
         indexPage
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    , dir "upload" $ uploadPage
    , dir "paste" $ path $ showPastePage
    ]

main :: IO ()
main = (forkIO runCleanup) >> simpleHTTP nullConf kopasteApp
