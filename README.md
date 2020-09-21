# `discord-haskell-extensions`

Originally just a personal bot written in Haskell for my own learning; now growing into a proper
abstraction layer over the lower-level `discord-haskell` library so that common Discord 
bot features, specifically commands, can be more quickly and easily written.

## Features
- Easy command definition interface/DSL using `do` notation. Supports subcommands, aliasing, and referring to previously-defined commands in subsequent hierarchy levels.

```haskell
{- example/Bot/Cogs/Base.hs -}

baseCmds :: Commands
baseCmds = do
  helpcmd <- cmd0 "help" cmdHelp "Shows help message." "Usage: help"
  cmd1 "greet" cmdGreet "Greets a user." "Usage: greet [@user]. Greets a user."
  cmd0 "bark" cmdBark "Barks at you." "It just barks. How cute!"
  saycmd <- cmds "say" cmdSayDefault "Says something." "Usage: say/speak <ayy|help>." $ do
    cmd0 "ayy" cmdSayAyy "Responds to ayy." "Responds to ayy with lmao."
    refer helpcmd
  alias "speak" saycmd
  cmd2 "phrases" cmd2Phrases "Underlines two phrases." "Usage: phrase [text1] [text2], quotes supported." 

```

- Bot commands with strongly-typed arguments that automatically get converted from text 
(resolved using user-provided type signatures), 
plus a custom `Mentionable` typeclass to make any type a possible argument type for a bot command.


```haskell
{- example/Bot/Cogs/Base.hs -}

-- registration of commands in the monadic block...
cmd1 "greet" cmdGreet "Greets a user." "Usage: greet [@user]. Greets a user."

cmd2 "phrases" cmd2Phrases "Underlines two phrases." "Usage: phrases [text1] [text2], quotes supported." 


-- ...along with the corresponding top-level function definitions
cmdGreet :: Context -> User -> DiscordHandler ()
cmdGreet ctx user = restCreateMessage ctx $ "Hi, " <> userName user <> "!"

cmd2Phrases :: Context -> T.Text -> T.Text -> DiscordHandler ()
cmd2Phrases ctx t1 t2 = restCreateMessage ctx $ "__" <> t1 <> "__ and __" <> t2 <> "__"
```

- Separation of functionality and related commands into cogs (WIP)
```haskell
{- example/Bot/ProcessMessage.hs -}

import Cogs.Base (baseCog)
import Cogs.Moderation (moderationCog)
import Cogs.Games (gamesCog)

cogs :: [Commands]
cogs = [baseCog, moderationCog, gamesCog]
```

## Possible next steps

- Support for optional (`Maybe`) and union (`Either`) argument types for command functions
- Owner and role checks for commands
- A way to expose help details of the command hierarchy to easily walk through and build a custom help command
- Support for "menus" that listen to reaction "buttons" for user interactions
- Helper module for persistent bot memory and data storage
- Voice and music-botting support (this isn't in `discord-haskell` so I'll have to roll up my own implementation, which is potentially difficult)
- Sharding?

## Inspiration

My original bot (Noglobot) started out as a toy `discord.py` bot. As a result, a lot of these features were inspired by functionality from `discord.py`, namely cogs, and automatic argument parsing based on function type signatures.