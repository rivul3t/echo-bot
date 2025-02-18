{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  (run,
   Handle (..)
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot as Bot
import qualified Data.Map as Map

newtype Handle = Handle
  { hBotHandle :: Bot.Handle
  }

run h = do
  
