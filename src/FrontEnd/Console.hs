{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot as Bot
--import qualified Logger (Level(..))
--import qualified Logger.Impl
--import qualified System.IO as SIO

newtype Handle = Handle
  { hBotHandle :: Bot.Handle IO T.Text
  }

run :: Handle -> IO ()
run h = do
  TIO.putStrLn "Welcome to the echo-bot!"
  messageCycle h

messageCycle :: Handle -> IO ()
messageCycle h = do
  input <- getLine -- 1. Read a line from the console.
  if input == "exit" then 
                       pure ()
                     else do

                   -- 2. Send it to the bot, get its response and output it.
    response <- Bot.respond (hBotHandle h) (Bot.MessageEvent $ T.pack input)
    case head response of
      Bot.MenuResponse title countList -> handleMenuResponse h title countList
      _                                -> handleEchoResponse response

  
                   -- 3. Go to 1.
    messageCycle h
 

handleEchoResponse :: [Bot.Response T.Text] -> IO ()
handleEchoResponse [] = pure ()
handleEchoResponse (Bot.MessageResponse response : xs) = do
  putStrLn . T.unpack $ response
  handleEchoResponse xs
handleEchoResponse (Bot.MenuResponse _ _ : _) = pure () -- TODO: fix this gap

handleMenuResponse :: Handle -> T.Text -> [(Int, Bot.Event T.Text)] -> IO ()
handleMenuResponse h title response = do
  TIO.putStrLn title
  mapM_ (TIO.putStrLn . T.pack . show . fst) response
  newCount <- TIO.getLine
  case lookup (read . T.unpack $ newCount) response of 
    Nothing -> TIO.putStrLn "Incorrect value"
    Just setRepetitionCount -> do
      _  <- Bot.respond (hBotHandle h) setRepetitionCount
      pure ()


