module ProcessCommands
  ( Context
  , CmdFunc
  , CmdFunc1
  , Commands
  , cmds
  , cmd0
  , cmd1
  , alias
  , refer
  , restCreateMessage
  , processCommands
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Text as T
import Data.Char (isSpace)

import Discord
import Discord.Types
import Discord.Requests as DR

-- | Context data type. Wraps the handle (for IO) and the Message 
-- that invoked the command
type Context = (DiscordHandle, Message)

-- | Shorthand synonym for the type of a user-defined IO function for a
-- zero-argument or subdivided command
type CmdFunc = Context -> IO ()

-- | Shorthand synonym for the type of a user-defined IO function for a 
-- command that requires a literal argument. 
-- Note that the argument is always passed in as a raw Text. 
-- TODO: allow specifying how to parse this Text for multiple arguments
type CmdFunc1 = Context -> T.Text -> IO ()

-- | Command data type. Holds information on how the user-defined IO function
-- should be executed and whether there are subcommands.
data Command = 
    Cmds CmdFunc CommandTree
  | Cmd0 CmdFunc
  | Cmd1 CmdFunc1

-- | PrettyCommand wrapper for a Command, for use in e.g. help messages.
data PrettyCommand =
  PrettyCmd { name :: T.Text
            , command :: Command
            , synopsis :: T.Text
            , description :: T.Text}

-- | Internal representation of the command hierarchy.
-- TODO: change this to something faster maybe?
type CommandTree = [PrettyCommand]

-- | A map to map aliases to the "main" names of commands.
-- The "main" name is the one that is specified in the cmd definition in the 
-- monadic block.
type AliasMap = M.Map T.Text T.Text

-- | A monad for building up the state of the command hierarchy and aliases. 
-- This is to make use of do notation to make the user's life easier 
-- when defining a custom command hierarchy.
type Commands = State (CommandTree, AliasMap) PrettyCommand

-- | Update the state with a new command and add alias accordingly
addAndReturnCmd :: PrettyCommand -> Commands
addAndReturnCmd c = let n = name c in
  modify (\(cs,as) -> (c:cs,M.insert n n as)) *> pure c

-- | A command with subcommands
cmds :: T.Text -> CmdFunc -> T.Text -> T.Text -> Commands -> Commands
cmds name func syn desc subsState = let (subs, aliases) = execState subsState ([],M.empty) in do
  returnCmd <- addAndReturnCmd (PrettyCmd name (Cmds func subs) syn desc)
  modify (\(cs,as) -> (cs,M.union aliases as))
  pure returnCmd

-- | A command with no arguments
cmd0 :: T.Text -> CmdFunc -> T.Text -> T.Text -> Commands
cmd0 name func syn desc = addAndReturnCmd $ PrettyCmd name (Cmd0 func) syn desc

-- | A command with argument(s)
cmd1 :: T.Text -> CmdFunc1 -> T.Text -> T.Text -> Commands
cmd1 name func syn desc = addAndReturnCmd $ PrettyCmd name (Cmd1 func) syn desc

-- | Reuse a command (used to make a command also a subcommand of 
-- another command on the same level)
refer :: PrettyCommand -> Commands
refer = addAndReturnCmd

-- | Set an alias for a command
alias :: T.Text -> PrettyCommand -> Commands
alias a c = modify (\(cs,as) -> (cs,M.insert a (name c) as)) *> pure c

-- Convenience functions to wrap common requests
restCreateMessage :: Context -> T.Text -> IO ()
restCreateMessage (dis, msg) t = 
  (restCall dis $ DR.CreateMessage (messageChannel msg) t) *> pure ()

-- | Parse and try to evaluate the command into an IO action
parseAndEvalCommand :: Context -> CommandTree -> AliasMap -> T.Text -> Either String (IO ())
parseAndEvalCommand ctx [] _ remains = 
  Left $ "No command was defined"
parseAndEvalCommand ctx (cmd:cmds) aliases remains =
  case M.lookup prefixText aliases of
    Nothing -> Left $ "Command not found; parsed up to '" ++ T.unpack remains ++ "' before failing"
    Just mainName -> case mainName == name cmd of
      False -> parseAndEvalCommand ctx cmds aliases remains
      True -> case command cmd of
        Cmd1 _ -> Right $ runCmdIOWith suffixText
        Cmd0 _ -> Right $ runCmdIOWith (T.empty)
        Cmds _ subCmds -> case T.strip suffixText == T.empty of
          True  -> Right $ runCmdIOWith (T.empty)
          False -> case parseAndEvalCommand ctx subCmds aliases suffixText of
            Left errorMsg   -> Right $ runCmdIOWith (T.empty)
            Right ioResult  -> Right ioResult
    where 
  prefixText = T.takeWhile (not . isSpace) remains
  suffixText = T.drop (1 + T.length prefixText) remains
  runCmdIOWith = runCmdIO ctx (command cmd)

-- | Do the user-defined IO () function for the command
runCmdIO :: Context -> Command -> T.Text -> IO ()
runCmdIO ctx (Cmds func _) _ = func ctx
runCmdIO ctx (Cmd0 func) _ = func ctx
runCmdIO ctx (Cmd1 func) argtext = func ctx argtext

-- | Run the IO action eval'd from the command, or report if invalid command
executeEvaluatedCommand :: Either String (IO ()) -> IO ()
executeEvaluatedCommand (Left errorMsg) = putStrLn errorMsg
executeEvaluatedCommand (Right ioResult) = ioResult

-- | IO action to encapsulate all the command processing on MessageCreate
processCommands :: Context -> Commands -> T.Text -> IO ()
processCommands ctx cmdsState t = let (cmdTree, aliases) = execState cmdsState ([],M.empty) in
  executeEvaluatedCommand $ parseAndEvalCommand ctx cmdTree aliases t
