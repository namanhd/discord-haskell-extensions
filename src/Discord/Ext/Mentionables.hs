module Discord.Ext.Mentionables
  ( Mentionable
  , getId
  , toMention
  , fromMention
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Text as T
import Text.Read (readMaybe)

import Discord
import Discord.Types
import Discord.Requests as DR

-- | A typeclass for any type that can be mentioned or 
-- parsed from a command or message
class Mentionable a where
  getId       :: a -> Snowflake
  toMention   :: a -> T.Text
  fromMention :: T.Text -> MaybeT DiscordHandler a

instance Mentionable User where
  getId = userId
  toMention a = T.pack $ "<@!" ++ show (getId a) ++ ">"
  fromMention t = do -- TODO: a discordpy-converter-ish case where t is just the nickname, not the whole mention
    flake <- parseUserFlake t
    result <- lift $ restCall $ DR.GetUser $ flake
    case result of
      Right user -> pure user
      _ -> (lift . lift $ putStrLn "[fromMention User] Error in restCall: User not found") *> (MaybeT $ pure Nothing)

instance Mentionable Channel where
  getId = channelId
  toMention c = T.pack $ "<#!" ++ show (getId c) ++ ">"
  fromMention t = do
    flake <- parseChannelFlake t
    result <- lift $ restCall $ DR.GetChannel $ flake
    case result of
      Right channel -> pure channel
      _ -> (lift . lift $ putStrLn "[fromMention Channel] Error in restCall: Channel not found") *> (MaybeT $ pure Nothing)

instance Mentionable Integer where
  getId = const 0
  toMention = T.pack . show
  fromMention = MaybeT . pure . readMaybe . T.unpack

instance Mentionable T.Text where
  getId = const 0
  toMention = id
  fromMention = MaybeT . pure . Just

instance Mentionable Double where
  getId = const 0
  toMention = T.pack . show
  fromMention = MaybeT . pure . readMaybe . T.unpack

-- TODO: add a Mentionable instance for Roles and Emojis (or whatever they're called in the lib)

-- | Get snowflake from things of the form <c!FLAKE> 
-- where c is a char, and FLAKE is a Word64
parseMentionedFlake :: Char -> T.Text -> MaybeT DiscordHandler Snowflake
parseMentionedFlake c t = MaybeT . pure $ T.stripSuffix (T.pack ">") t
  >>= T.stripPrefix (T.pack $ '<':c:['!']) 
  >>= readMaybe . T.unpack

-- | Convenience parse functions for each type of flake
parseUserFlake :: T.Text -> MaybeT DiscordHandler Snowflake
parseUserFlake = parseMentionedFlake '@'

parseChannelFlake :: T.Text -> MaybeT DiscordHandler Snowflake
parseChannelFlake = parseMentionedFlake '#'
