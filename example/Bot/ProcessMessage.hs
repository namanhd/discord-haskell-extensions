module Bot.ProcessMessage where

import Data.Text as T

import Discord
import Discord.Types

import Discord.Ext.Commands
import Bot.Cogs.Base (baseCmds)
-- import other cog modules here

cogs :: [Commands]
cogs = [baseCmds] -- add the other cogs' Commands state function here

processMsg :: Message -> T.Text -> DiscordHandler ()
processMsg msg cmdPrefix = 
  processPossibleCommand msg cmdPrefix cogs processNonCommands 

processNonCommands :: DiscordHandler ()
processNonCommands = pure ()