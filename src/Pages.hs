{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Pages (
  indexPage
  , uploadPage
  , showPastePage
) where

import Control.Monad.IO.Class

import Control.Applicative (optional)
import Control.Exception

import Data.Text.Lazy (unpack)
import Data.Text (pack)

import System.Directory

import Test.RandomStrings

import Happstack.Server as HP

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA

showPastePage :: String -> ServerPartT IO Response
showPastePage pasteName = do
  pstCode :: Either IOException String <-
    liftIO $ try (readFile ("pastes/" ++ pasteName))
  maybeLangHint <- optional $ lookText "lang"
  langHint <- return $ fmap (\l -> unpack l) maybeLangHint
  case pstCode of
    Left e -> notFound $ toResponse ("Paste " ++ pasteName ++ " does not exist")
    Right pasteCode ->
      ok $
      toResponse $
      html $ do
        H.head $ do
          H.title $ preEscapedString ("Paste " ++ pasteName)
          H.link ! rel "stylesheet" !
            href "/static/highlight-styles/dracula.css"
          H.script ! src "/static/highlight.pack.js" $ "" -- Load highlight.js
          H.script $ preEscapedString "hljs.initHighlightingOnLoad();"
        H.body $ do H.pre $ H.code ! class_ (case langHint of
                                               Just l -> textValue (pack $ "lang-" ++ l)
                                               Nothing -> "") $ string pasteCode


-- Generate a random name for a paste, if such a paste already exists - generate a new name
genRandomName :: ServerPartT IO String
genRandomName = do
  randName <- liftIO $ randomString (onlyAlphaNum randomASCII) 6
  fileExists <- liftIO $ doesFileExist ("pastes/" ++ randName)
  if not fileExists
    then return randName
    else genRandomName

uploadPage :: ServerPartT IO Response
uploadPage = do
  HP.method POST -- Match on POST method
  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
  pastedText <- lookText "text"
  pasteName <- genRandomName
  liftIO $ do
    createDirectoryIfMissing False "pastes"
    writeFile ("pastes/" ++ pasteName) (unpack pastedText)
  seeOtherResp <- seeOther ("/paste/" ++ pasteName) pasteName
  return (toResponse seeOtherResp)

indexPage :: ServerPartT IO Response
indexPage =
  ok $
  toResponse $
  html $ do
    H.head $ do H.title "Kopaste"
    H.body $ do
      h1 ! class_ "name-header" $ "Kopaste"
      H.form ! HA.id "text-form" ! action "/upload" ! HA.method "POST" !
        enctype "multipart/form-data" $ do
        textarea ! type_ "text" ! name "text" ! HA.form "text-form" ! rows "25" !
          cols "100" $
          ""
        br >> input ! type_ "submit" ! value "Paste!"
