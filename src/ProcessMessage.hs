module ProcessMessage where

import Data.Text as T

import Discord
import Discord.Types

import Ext.Commands
import Cogs.Base (baseCmds)
-- import other cog modules here

cogs :: [Commands]
cogs = [baseCmds] -- add the other cogs' Commands state function here

processMsg :: Context -> T.Text -> IO ()
processMsg ctx@(dis, msg) cmdPrefix = case T.stripPrefix cmdPrefix (messageText msg) of
  Just cmdText -> processCommands ctx cogs cmdText
  Nothing -> processNonCommands ctx

processNonCommands :: Context -> IO ()
processNonCommands ctx = pure ()