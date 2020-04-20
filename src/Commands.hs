{-# LANGUAGE OverloadedStrings #-}

module Commands (
    myCmds
) where

import Data.Text as T
import ProcessCommands

-- With the machinery we have in ProcessCommands, the user can define
-- commands like this:

myCmds :: Commands
myCmds = do
  helpcmd <- cmd0 "help" cmdHelp "Shows help message" "No help right now!"
  cmd1 "repeat" cmdRepeat "Repeats a message" "Repeats a message. Everything after 'repeat' is repeated."
  saycmd <- cmds "say" cmdSayDefault "Says something" "Subcommands: 'marco', 'ayy'."$ do
    cmd0 "marco" cmdSayMarco "Responds to marco" "Responds to marco with polo"
    cmd0 "ayy" cmdSayAyy "Responds to ayy" "Responds to ayy with lmao"
    refer helpcmd
  alias "speak" saycmd

cmdHelp :: CmdFunc
cmdHelp ctx = restCreateMessage ctx $ T.pack "I can't help you, I'm just a bebe"

cmdSayDefault :: CmdFunc
cmdSayDefault ctx = restCreateMessage ctx $ T.pack "I don't know what to say"

cmdSayMarco :: CmdFunc
cmdSayMarco ctx = restCreateMessage ctx $ T.pack "polo"

cmdSayAyy :: CmdFunc
cmdSayAyy ctx = restCreateMessage ctx $ T.pack "lmao"

cmdRepeat :: CmdFunc1
cmdRepeat ctx argtext = restCreateMessage ctx argtext
