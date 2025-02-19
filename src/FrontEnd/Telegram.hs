{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module FrontEnd.Telegram
  (run,
   Handle (..)
  )
where

import qualified Data.Text as T
import qualified EchoBot as Bot
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as NS
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
--import Config (getTelegramApi)

getTelegramApi :: IO String
getTelegramApi = pure $ "https://api.telegram.org/botYOURTOKEN/"

data TelegramResponse = TelegramResponse
  { ok :: Bool
  , result :: [Update]
  } deriving (Show, Generic)

instance FromJSON TelegramResponse

data Update = Update
  { updateId :: Int
  , message :: Maybe Message
  } deriving (Show, Generic)

instance FromJSON Update

data Message = Message
  { messageId :: Int
  , from :: User
  , chat :: Chat
  , date :: Int
  , text :: Maybe T.Text
  , photo :: Maybe [PhotoSize]
  } deriving (Show, Generic)

instance FromJSON Message

data User = User
  { userId :: Int
  , isBot :: Bool
  , firstName :: T.Text
  , username :: T.Text
  , languageCode :: T.Text
  } deriving (Show, Generic)

instance FromJSON User

data Chat = Chat
  { chatId :: Int
  , chatFirstName :: T.Text
  , chatUsername :: T.Text
  , chatType :: T.Text
  } deriving (Show, Generic)

instance FromJSON Chat

data PhotoSize = PhotoSize
  { fileId :: T.Text
  , fileUniqueId :: T.Text
  , fileSize :: Int
  , width :: Int
  , height :: Int
  } deriving (Show, Generic)

instance FromJSON PhotoSize

newtype Handle = Handle
  { hBotHandle :: Bot.Handle IO String
  }

run :: Handle -> IO ()
run stubHandle = do
  api <- getTelegramApi
  messageCycle HM.empty stubHandle api
  
type TelegramApi = String

messageCycle :: HM.HashMap Int Handle -> Handle -> TelegramApi -> IO ()
messageCycle handles stubHandle telegramApi = do
  updates <- getUpdates telegramApi
  case ok updates of
    False -> messageCycle handles stubHandle telegramApi
    True  -> do
      pure ()
 
getUpdates :: TelegramApi -> IO TelegramResponse
getUpdates api = do
  request <- NS.parseRequest $ api ++ "getUpdates"
  response <- NS.httpLBS $ NS.setRequestQueryString [("timeout", Just "30")] request
  case decode $ NS.getResponseBody response of
    Nothing -> pure $ TelegramResponse { ok = False, result = [] }
    Just answer -> pure answer
  
