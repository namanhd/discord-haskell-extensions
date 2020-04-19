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

processNonCommands :: Context -> IO ()
processNonCommands ctx = pure ()