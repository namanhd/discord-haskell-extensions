module ProcessCommands (
  Context,
  CmdFunc,
  CmdFunc1,
  CommandTree,
  cmds,
  cmd0,
  cmd1,
  restCreateMessage,
  processCommands
) where

import Data.Text as T
import Data.Char (isSpace)
import Discord
import Discord.Types
import Discord.Requests as DR

type Context = (DiscordHandle, Message)
type CmdFunc = Context -> IO ()
-- CmdFunc1 denotes a function/command that requires a literal argument. 
-- This is always passed in as a raw Text. The converting is done inside each function,
-- unlike discord.py's convert-on-pass mechanism.
type CmdFunc1 = Context -> T.Text -> IO ()

data Command = 
    Cmds String CmdFunc CommandTree
  | Cmd0 String CmdFunc
  | Cmd1 String CmdFunc1

type CommandTree = [Command]

cmdName :: Command -> T.Text
cmdName (Cmds nameStr _ _) = T.pack nameStr
cmdName (Cmd0 nameStr _) = T.pack nameStr
cmdName (Cmd1 nameStr _) = T.pack nameStr

-- | A command with subcommands
cmds :: String -> CmdFunc -> CommandTree -> Command
cmds = Cmds

-- | An atomic command with no arguments
cmd0 :: String -> CmdFunc -> Command
cmd0 = Cmd0

-- | An atomic command with a concrete argument
cmd1 :: String -> CmdFunc1 -> Command
cmd1 = Cmd1

-- Convenience functions to wrap common requests
restCreateMessage :: Context -> T.Text -> IO ()
restCreateMessage (dis, msg) t = 
  (restCall dis $ DR.CreateMessage (messageChannel msg) t) *> pure ()

parseAndEvalCommand :: Context -> CommandTree -> T.Text -> Either String (IO ())
parseAndEvalCommand ctx [] remains = 
  Left $ "Command not found; parsed up to '" ++ T.unpack remains ++ "' before failing"
parseAndEvalCommand ctx (cmd:cmds) remains =
  if cmdName cmd /= prefixText 
  then parseAndEvalCommand ctx cmds remains
  else case cmd of
    Cmd1 _ _ -> Right $ runCmdIOWith suffixText
    Cmd0 _ _ -> Right $ runCmdIOWith (T.empty)
    Cmds _ _ subcmds -> case T.strip suffixText == T.empty of
      True  -> Right $ runCmdIOWith (T.empty)
      False -> case parseAndEvalCommand ctx subcmds suffixText of
        Left errorMsg   -> Right $ runCmdIOWith (T.empty)
        Right ioResult  -> Right ioResult
    where
  prefixText = T.takeWhile (not . isSpace) remains
  suffixText = T.drop (1 + T.length prefixText) remains
  runCmdIOWith = runCmdIO ctx cmd

runCmdIO :: Context -> Command -> T.Text -> IO ()
runCmdIO ctx (Cmds name func _) _ = func ctx
runCmdIO ctx (Cmd0 name func) _ = func ctx
runCmdIO ctx (Cmd1 name func) argtext = func ctx argtext

executeEvaluatedCommand :: Either String (IO ()) -> IO ()
executeEvaluatedCommand (Left errorMsg) = putStrLn errorMsg
executeEvaluatedCommand (Right ioResult) = ioResult

processCommands :: Context -> CommandTree -> T.Text -> IO ()
processCommands ctx cmdTree = executeEvaluatedCommand . parseAndEvalCommand ctx cmdTree
