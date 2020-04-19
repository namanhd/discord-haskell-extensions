module ProcessMessage where

import Data.Text as T

import Discord
import Discord.Types

import ProcessCommands
import Commands (myCmdTree)

processMsg :: Context -> T.Text -> IO ()
processMsg ctx@(dis, msg) cmdPrefix = case T.stripPrefix cmdPrefix (messageText msg) of
  Just cmdText -> processCommands ctx myCmdTree cmdText
  Nothing -> processNonCommands ctx
  -- if (cmdPrefix `isPrefixOf` messageText msg) 
  -- then processCommands ctx
  -- else processNonCommands ctx

-- processCommands :: Context -> T.Text -> IO ()
-- processCommands ctx@(dis, msg) cmdPrefix = case cmdText of
--   Just cmdText -> doProcessCommands ctx myCmdTree cmdText
--   Nothing -> putStrLn "Command went through processMsg but didn't go through"
--     where
--   content = messageText msg
--   cmdText = T.stripPrefix cmdPrefix $ content 

processNonCommands :: Context -> IO ()
processNonCommands ctx = pure ()