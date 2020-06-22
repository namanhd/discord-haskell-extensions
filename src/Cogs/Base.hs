{-# LANGUAGE OverloadedStrings #-}

module Cogs.Base
  ( baseCmds
  ) where

import qualified Data.Text as T

import Discord.Types

import Ext.Commands

-- With the machinery we have in ProcessCommands, the user can define
-- commands like this:

baseCmds :: Commands
baseCmds = do
  helpcmd <- cmd0 "help" cmdHelp "Shows help message." "Usage: help"
  cmd1 "greet" cmdGreet "Greets a user." "Usage: greet [@user]. Greets a user."
  cmd0 "bark" cmdBark "Barks at you." "It just barks. How cute!"
  cmdA "repeat" cmdRepeat "Repeats a message" "Everything after 'repeat' is repeated."
  saycmd <- cmds "say" cmdSayDefault "Says something." "Usage: say <marco|ayy|help>" $ do
    cmd0 "marco" cmdSayMarco "Responds to marco." "Responds to marco with polo."
    cmd0 "ayy" cmdSayAyy "Responds to ayy." "Responds to ayy with lmao."
    refer helpcmd
  alias "speak" saycmd
  cmd2 "phrases" cmd2Phrases "Underlines two phrases." "Usage: phrases [text1] [text2], quotes supported" 

cmdBark :: Context -> IO ()
cmdBark ctx = restCreateMessage ctx "Woof! (Noglobot Haskell barks first!)"

cmdHelp :: Context -> IO ()
cmdHelp ctx = restCreateMessage ctx "I can't help you, I'm just a bebe"

cmdSayDefault :: Context -> IO ()
cmdSayDefault ctx = restCreateMessage ctx "I don't know what to say"

cmdSayMarco :: Context -> IO ()
cmdSayMarco ctx = restCreateMessage ctx "polo"

cmdSayAyy :: Context -> IO ()
cmdSayAyy ctx = restCreateMessage ctx "lmao"

cmdRepeat :: Context -> T.Text -> IO ()
cmdRepeat ctx argtext = restCreateMessage ctx argtext

cmdGreet :: Context -> User -> IO ()
cmdGreet ctx user = restCreateMessage ctx $ "Hi, " <> userName user <> "!"

cmd2Phrases :: Context -> T.Text -> T.Text -> IO ()
cmd2Phrases ctx t1 t2 = restCreateMessage ctx $ "__" <> t1 <> "__ and __" <> t2 <> "__"