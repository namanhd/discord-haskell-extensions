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
        cmd0 "marco" cmdSayLigma,
        cmd0 "ayy" cmdSaySugma
      ]
  ]

cmdHelp :: CmdFunc
cmdHelp ctx = restCreateMessage ctx $ T.pack "I can't help you, I'm just a bebe"

cmdSayDefault :: CmdFunc
cmdSayDefault ctx = restCreateMessage ctx $ T.pack "I don't know what to say"

cmdSayLigma :: CmdFunc
cmdSayLigma ctx = restCreateMessage ctx $ T.pack "polo"

cmdSaySugma :: CmdFunc
cmdSaySugma ctx = restCreateMessage ctx $ T.pack "lmao"

cmdRepeat :: CmdFunc1
cmdRepeat ctx argtext = restCreateMessage ctx argtext
