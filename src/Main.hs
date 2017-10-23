{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad (msum)

import Happstack.Server

import Pages

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
main = simpleHTTP nullConf kopasteApp
