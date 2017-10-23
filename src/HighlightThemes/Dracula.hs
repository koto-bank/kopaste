-- This is a port of highlight.js' dracula theme
-- Original: https://github.com/dracula/highlightjs

{-# LANGUAGE OverloadedStrings #-}

module HighlightThemes.Dracula
  ( draculaTheme
  ) where

import Prelude hiding ((**))

import Data.Monoid ((<>))

import Clay hiding (map)

import Data.Text.Lazy (unpack)

draculaTheme :: String
draculaTheme =
  unpack $
  renderWith compact [] $ do
    ".hljs" ? do
      display block
      overflowX auto
      padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
      background ("#282a36" :: Color)
    (".hljs-built_in" <> ".hljs-selector-tag" <> ".hljs-section" <> ".hljs-link") ?
      color "#8be9fd"
    "hljs-keyword" ? color "#ff79c6"
    (".hljs" <> ".hljs-subst") ? color "#f8f8f2"
    ".hljs-title" ? color "#50fa7b"
    (".hljs-string" <> ".hljs-meta" <> ".hljs-name" <> ".hljs-type" <>
     ".hljs-attr" <>
     ".hljs-symbol" <>
     ".hljs-bullet" <>
     ".hljs-addition" <>
     ".hljs-variable" <>
     ".hljs-template-tag" <>
     ".hljs-template-variable") ?
      color "#f1fa8c"
    (".hljs-comment" <> ".hljs-quote" <> ".hljs-deletion") ? color "#6272a4"
    ("hljs-keyword" <> ".hljs-selector-tag" <> ".hljs-literal" <> ".hljs-title" <>
     ".hljs-section" <>
     ".hljs-doctag" <>
     ".hljs-type" <>
     ".hljs-name" <>
     ".hljs-strong") ?
      fontWeight bold
    (".hljs-literal" <> ".hljs-number") ? color "#bd93f9"
    ".hljs-emphasis" ? fontStyle italic
