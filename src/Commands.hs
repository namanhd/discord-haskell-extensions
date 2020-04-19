module Commands (
    myCmdTree
) where


import Data.Text as T
import ProcessCommands

-- Now, with this machinery, we can do stuff like this:
myCmdTree :: CommandTree
myCmdTree = 
  [
    cmd0 "help" cmdHelp,
    cmd1 "repeat" cmdRepeat,
    cmds  "say" cmdSayDefault 
      [
        cmd0 "ligma" cmdSayLigma,
        cmd0 "sugma" cmdSaySugma
      ]
  ]

cmdHelp :: CmdFunc
cmdHelp ctx = restCreateMessage ctx $ T.pack "I can't help you, I'm just a bebe"

cmdSayDefault :: CmdFunc
cmdSayDefault ctx = restCreateMessage ctx $ T.pack "No ligging here"

cmdSayLigma :: CmdFunc
cmdSayLigma ctx = restCreateMessage ctx $ T.pack "balls"

cmdSaySugma :: CmdFunc
cmdSaySugma ctx = restCreateMessage ctx $ T.pack "dicc"

cmdRepeat :: CmdFunc1
cmdRepeat ctx argtext = restCreateMessage ctx argtext
