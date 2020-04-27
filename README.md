# Noglobot Haskell

Originally a personal bot written mostly for my own learning; now more like a general
abstraction layer over the lower-level `discord-haskell` library so that common Discord 
bot features, specifically commands, can be more quickly written.

## Features
- Easy command definition interface using `do` notation. Supports subcommands, aliasing, and referring to previously-defined commands.

```haskell
{- Cogs/Base.hs -}

baseCmds :: Commands
baseCmds = do
  helpcmd <- cmd0 "help" cmdHelp "Shows help message." "Usage: help"
  cmd1 "greet" cmdGreet "Greets a user." "Usage: greet [@user]. Greets a user."
  cmd0 "bark" cmdBark "Barks at you." "It just barks. How cute!"
  saycmd <- cmds "say" cmdSayDefault "Says something." "Usage: say/speak <ayy|help>." $ do
    cmd0 "ayy" cmdSayAyy "Responds to ayy." "Responds to ayy with lmao."
    refer helpcmd
  alias "speak" saycmd
```

- Bot commands with typed arguments and automatic conversion from text, plus a custom `Mentionable` typeclass to make any type a possible argument type for a bot command.

```haskell
{- Cogs/Base.hs -}

-- defining the command in baseCmds:
cmd1 "greet" cmdGreet "Greets a user." "Usage: greet [@user]. Greets a user."

-- defining the command's function:
cmdGreet :: Context -> User -> IO ()
```

- Separation of functionality and related commands into cogs (WIP)
```haskell
{- ProcessMessage.hs -}

import Cogs.Base (baseCog)
import Cogs.Moderation (moderationCog)
import Cogs.Games (gamesCog)

cogs :: [Commands]
cogs = [baseCog, moderationCog, gamesCog]
```

## Planned features

- Support for "menus" that listen to reaction "buttons" for user interactions
- Owner and role checks for commands
- A function to expose help details of the command hierarchy to easily walk through commands and build a custom help command
- Voice and music-botting support (this isn't in `discord-haskell` so I'll have to roll up my own implementation, which is potentially difficult)

## Inspiration

Noglobot started and still is maintained as a `discord.py` bot. As a result, a lot of these features were inspired by functionality from `discord.py`, namely the use of converters and cogs.