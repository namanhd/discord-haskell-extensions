module Main where

import Control.Monad.Trans (lift)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import Discord.Requests as DR
import System.Environment (getEnv)

import Bot.ProcessMessage

cmdPrefix :: T.Text
cmdPrefix = T.pack "dm "

main :: IO ()
main = botMain

-- | The main function that initializes the bot
botMain :: IO ()
botMain = do
  tokenStr <- getEnv "DISCORD_BOT_TOKEN"
  let token = T.pack tokenStr
  t <- runDiscord $ def 
    { discordToken = token
    , discordOnStart = handlerOnReady
    , discordOnEnd = putStrLn "Bot shut down"
    , discordOnEvent = handlerOnEvent
    , discordOnLog = \txt -> TIO.putStrLn txt
    }
  TIO.putStrLn t

-- | Run at bot init time
handlerOnReady :: DiscordHandler ()
handlerOnReady = do
  Right user <- restCall DR.GetCurrentUser
  lift $ TIO.putStrLn $ (T.pack "Logged on as ") <> userName user
  pure ()

-- | Run specific functions based on the incoming event
handlerOnEvent ::  Event -> DiscordHandler ()
handlerOnEvent event = case event of 
  MessageCreate m -> onMessageCreate m
  _ -> pure ()

-- | Run when a new message event is invoked
onMessageCreate :: Message -> DiscordHandler ()
onMessageCreate m = if userIsBot (messageAuthor m) then pure ()
  else processMsg m cmdPrefix