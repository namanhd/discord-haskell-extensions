{-# LANGUAGE GADTs #-}

module Discord.Ext.Commands
  ( Context
  , Commands
  , cmds
  , cmd0
  , cmdA
  , cmd1, cmd2, cmd3, cmd4
  , alias
  , refer
  , restCreateMessage
  , processCommands
  , processPossibleCommand
  ) where

import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Char (isSpace)

import Discord
import Discord.Types
import Discord.Requests as DR

import Discord.Ext.Mentionables

-- | Context data type. Wraps info about the message that invoked the command.
data Context = Context { disHandle :: DiscordHandle
                       , ctxMessage :: Message
                       , ctxAuthor :: User
                       , ctxChannelId :: ChannelId
                       , ctxCommandPrefix :: T.Text
                       }

type DH = DiscordHandler -- Shorthand for convenience

-- | Shorthand synonym for the type of a user-defined IO function for a
-- zero-argument or subdivided command
type CmdFunc0 = Context -> DH ()

-- | Shorthand synonym for the type of a user-defined IO function for a 
-- command that takes a greedy argument.
type CmdFuncA = Context -> T.Text -> DH ()

-- | A wrapper type for user-def IO functions for commands with multiple args
data MultiArgCommandFunction where
  CmdFunc1 :: (Mentionable a) =>
    (Context -> a -> DH ()) -> MultiArgCommandFunction
  CmdFunc2 :: (Mentionable a, Mentionable b) => 
    (Context -> a -> b -> DH ()) -> MultiArgCommandFunction
  CmdFunc3 :: (Mentionable a, Mentionable b, Mentionable c) => 
    (Context -> a -> b -> c -> DH ()) -> MultiArgCommandFunction
  CmdFunc4 :: (Mentionable a, Mentionable b, Mentionable c, Mentionable d) =>
    (Context -> a -> b -> c -> d -> DH ()) -> MultiArgCommandFunction

-- | Command data type. Holds information on how the user-defined IO function
-- should be executed and whether there are subcommands.
data Command = 
    Cmds CmdFunc0 CommandTree
  | Cmd0 CmdFunc0
  | CmdA CmdFuncA
  | CmdN MultiArgCommandFunction

-- | PrettyCommand wrapper for a Command, for use in e.g. help messages.
data PrettyCommand =
  PrettyCmd { name :: T.Text
            , command :: Command
            , synopsis :: T.Text
            , description :: T.Text
            }

-- | Internal representation of the command hierarchy.
-- TODO: change this to something faster maybe
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
cmds :: T.Text -> CmdFunc0 -> T.Text -> T.Text -> Commands -> Commands
cmds nm func syn desc subsState = let (subs, aliases) = execState subsState ([],M.empty) in do
  returnCmd <- addAndReturnCmd (PrettyCmd nm (Cmds func subs) syn desc)
  modify (\(cs,as) -> (cs,M.union aliases as))
  pure returnCmd

-- | Helper to factor out the addAndReturnCmd
registerInternalCmd :: T.Text -> Command -> T.Text -> T.Text -> Commands
registerInternalCmd nm internalCmd syn desc = 
  addAndReturnCmd $ PrettyCmd nm internalCmd syn desc

-- | A command with no arguments
cmd0 :: T.Text -> CmdFunc0 -> T.Text -> T.Text -> Commands
cmd0 nm func syn desc = registerInternalCmd nm (Cmd0 func) syn desc

-- | A command with a greedy argument. Everything after the command's name is the argument.
cmdA :: T.Text -> CmdFuncA -> T.Text -> T.Text -> Commands
cmdA nm func syn desc = registerInternalCmd nm (CmdA func) syn desc

-- | Multi-arg commands.
cmd1 :: Mentionable a =>
  T.Text -> (Context -> a -> DH ()) -> T.Text -> T.Text -> Commands
cmd2 :: (Mentionable a, Mentionable b) => 
  T.Text -> (Context -> a -> b -> DH ()) -> T.Text -> T.Text -> Commands
cmd3 :: (Mentionable a, Mentionable b, Mentionable c) => 
  T.Text -> (Context -> a -> b -> c -> DH ()) -> T.Text -> T.Text -> Commands
cmd4 :: (Mentionable a, Mentionable b, Mentionable c, Mentionable d) =>
  T.Text -> (Context -> a -> b -> c -> d -> DH ()) -> T.Text -> T.Text -> Commands

cmd1 nm func syn desc = registerInternalCmd nm (CmdN (CmdFunc1 func)) syn desc
cmd2 nm func syn desc = registerInternalCmd nm (CmdN (CmdFunc2 func)) syn desc
cmd3 nm func syn desc = registerInternalCmd nm (CmdN (CmdFunc3 func)) syn desc
cmd4 nm func syn desc = registerInternalCmd nm (CmdN (CmdFunc4 func)) syn desc

-- | Reuse a command (used to make a command also a subcommand of 
-- another command on the same level)
refer :: PrettyCommand -> Commands
refer = addAndReturnCmd

-- | Set an alias for a command
alias :: T.Text -> PrettyCommand -> Commands
alias a c = modify (\(cs,as) -> (cs,M.insert a (name c) as)) *> pure c

-- | =Convenience functions to wrap common requests=

-- | Create a message in the triggering command's channel
restCreateMessage :: Context -> T.Text -> DH ()
restCreateMessage ctx t = 
  (restCall $ DR.CreateMessage (ctxChannelId ctx) t) *> pure ()

-- TODO: add more of these (for things like embeds, reactions, etc)

-- | =Evaluating commands= 

-- | Get prefix text of command. This is whatever the inputted command name is 
-- (right after the command prefix, but before the first space)
getPrefixText :: T.Text -> T.Text
getPrefixText = T.takeWhile (not . isSpace)

-- | Find what the main name of a command is from its alias if possible
resolveCommandAlias :: AliasMap -> T.Text -> Either String T.Text
resolveCommandAlias aliasMap remains = 
  maybe (Left "[resolveCommandAlias] Input is not an alias of any recognized command") Right $ 
    M.lookup (getPrefixText remains) aliasMap
  
-- | Get the Command with the resolved name
getCommandFromResolvedName :: CommandTree -> T.Text -> Either String Command
getCommandFromResolvedName [] _ = Left "[getCommandFromResolvedName] No commands were defined"
getCommandFromResolvedName (c:cs) mainName = if mainName == name c
  then Right $ command c
  else getCommandFromResolvedName cs mainName

-- | Evaluate the found command based on what kind it is
evalCommand :: Context -> AliasMap -> T.Text -> Command -> Either String (DH ())
evalCommand ctx aliasMap remains cmd = case cmd of 
  CmdN _ -> Right $ runCmdIOWith suffixText
  CmdA _ -> Right $ runCmdIOWith suffixText
  Cmd0 _ -> Right $ runCmdIOWith (T.empty)
  Cmds _ subCmds -> if T.strip suffixText == T.empty
    then Right $ runCmdIOWith (T.empty)
    else either (const . Right $ runCmdIOWith (T.empty)) Right $
      parseAndEvalCommand ctx subCmds aliasMap suffixText
  where
    suffixText = T.drop (1 + T.length (getPrefixText remains)) remains
    runCmdIOWith = runCmdIO ctx cmd

-- | Sequences together above components to parse and evaluate a command string
-- into Either a wrapped IO action or a wrapped error message
parseAndEvalCommand :: Context -> CommandTree -> AliasMap -> T.Text -> Either String (DH ())
parseAndEvalCommand ctx cmdTree aliasMap remains = 
  resolveCommandAlias aliasMap remains 
  >>= getCommandFromResolvedName cmdTree 
  >>= evalCommand ctx aliasMap remains

-- | Do the user-defined IO function for the command
runCmdIO :: Context -> Command -> T.Text -> DH ()
runCmdIO ctx (Cmds func _) _ = func ctx
runCmdIO ctx (Cmd0 func) _ = func ctx
runCmdIO ctx (CmdA func) argtext = func ctx argtext
runCmdIO ctx (CmdN multiArgFunc) argtext = 
  runMultiArgIO ctx multiArgFunc $ splitPreserveQuotes argtext
  where
    splitOnlyOutOfQuotes :: Int -> [T.Text] -> [[T.Text]]
    splitOnlyOutOfQuotes _ [] = []
    splitOnlyOutOfQuotes i (sub:subs) =
      -- Even-indexed items in the output list are outside of quotes. Odds are inside. 
      (if (i `rem` 2 == 0) then T.split isSpace sub else [sub]) : splitOnlyOutOfQuotes (i+1) subs

    splitPreserveQuotes :: T.Text -> [T.Text]
    splitPreserveQuotes = 
      filter (not . T.null) . join . splitOnlyOutOfQuotes 0 . T.split (=='"') 

-- | Do the user-defined multi-argument IO function for the command
runMultiArgIO :: Context -> MultiArgCommandFunction -> [T.Text] -> DH ()
runMultiArgIO ctx multiArgFunc args = 
  runMaybeT runMultiArgIOMaybe *> pure ()
  where    
    runMultiArgIOMaybe :: MaybeT DH ()
    runMultiArgIOMaybe = do
      case (multiArgFunc, args) of
        (CmdFunc1 func, [a]) -> fromMention a >>= \u -> lift $ func ctx u
        (CmdFunc2 func, [a,b]) -> do
          u <- fromMention a
          v <- fromMention b 
          lift $ func ctx u v
        (CmdFunc3 func, [a,b,c]) -> do
          u <- fromMention a
          v <- fromMention b 
          w <- fromMention c 
          lift $ func ctx u v w
        (CmdFunc4 func, [a,b,c,d]) -> do
          u <- fromMention a
          v <- fromMention b 
          w <- fromMention c
          x <- fromMention d 
          lift $ func ctx u v w x
        _ -> lift $ -- lift from IO, to DH, to MaybeT DH
          lift $ putStrLn "[runMultiArgIO] Error in pattern match: Invalid number of arguments"

-- | Unpack and run the wrapped IO action eval'd from the command, 
-- or report to stdout if invalid
executeEvaluatedCommand :: Either String (DH ()) -> DH ()
executeEvaluatedCommand (Left errorMsg) = lift $ putStrLn errorMsg
executeEvaluatedCommand (Right ioResult) = ioResult

-- | Compiles cogs and executes all the command processing
processCommands :: Context -> [Commands] -> T.Text -> DH ()
processCommands ctx allCmdsStates t = 
    let (cmdTree, aliases) = execState (sequence_ allCmdsStates) mempty in
  executeEvaluatedCommand $ parseAndEvalCommand ctx cmdTree aliases t

-- | Entry point for triggering command processing on message
processPossibleCommand :: Message -> T.Text -> [Commands] -> DH () -> DH ()
processPossibleCommand msg cmdPrefix cogs actionOnNonCommand = do
  dis <- ask
  let ctx = Context dis msg (messageAuthor msg) (messageChannel msg) cmdPrefix 
  maybe actionOnNonCommand (processCommands ctx cogs) $ T.stripPrefix cmdPrefix (messageText msg)
