-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import qualified Logger
import Data.Text (pack)
import System.IO (openFile, IOMode(..))

-- | Gets the bot config. In any case it can provide reasonable
-- default values.

--getTelegramApi:: IO String
--getTelegramApi = pure $ "https://api.telegram.org/YOURTOKEN/"

getBotConfig :: IO EchoBot.Config
getBotConfig = 
  pure $ EchoBot.Config
    {
      EchoBot.confHelpReply = pack "This bot repeat your message, to change count use '/repeat' command",
      EchoBot.confRepeatReply = pack "Current repetition count is {count}",
      EchoBot.confRepetitionCount = 3,
      EchoBot.confMaxRepetition = 5
    }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  fileHandle <- openFile "logs.txt" AppendMode
  pure $ Logger.Impl.Config
    {
      Logger.Impl.confFileHandle = fileHandle,
      Logger.Impl.confMinLevel = Logger.Info
    }

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = 
  pure $ ConfigurationTypes.ConsoleFrontEnd
