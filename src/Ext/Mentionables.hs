module Ext.Mentionables
  (
      Mentionable
    , getId
    , toMention
    , fromMention
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Text as T
import Text.Read (readMaybe)

import Discord
import Discord.Internal.Types.Prelude (Snowflake)
import Discord.Types
import Discord.Requests as DR

-- | Defines a default "bad" Snowflake
unpackMaybeSnowflake :: Maybe Snowflake -> Snowflake
unpackMaybeSnowflake Nothing = Snowflake 0
unpackMaybeSnowflake (Just word) = word

-- | A typeclass for any type that can be mentioned or 
-- parsed from a command or message
class Mentionable a where
  getId       :: a -> Snowflake
  toMention   :: a -> T.Text
  fromMention :: DiscordHandle -> T.Text -> MaybeT IO a

instance Mentionable User where
  getId = userId
  toMention a = T.pack $ "<@!" ++ show (getId a) ++ ">"
  fromMention dis t = do -- TODO: a discordpy-converter-ish case where t is just the nickname, not the whole mention
    let flake = parseUserFlake t
    result <- lift $ restCall dis $ DR.GetUser $ parseUserFlake $ t
    case result of
      Right user -> pure user
      _ -> lift (putStrLn "[fromMention User] Error in restCall: User not found") *> (MaybeT $ pure Nothing)

instance Mentionable Channel where
  getId = channelId
  toMention c = T.pack $ "<#!" ++ show (getId c) ++ ">"
  fromMention dis t = do
    result <- lift $ restCall dis $ DR.GetChannel $ parseChannelFlake $ t
    case result of
      Right channel -> pure channel
      _ -> lift (putStrLn "[fromMention Channel] Error in restCall: Channel not found") *> (MaybeT $ pure Nothing)

instance Mentionable Integer where
  getId = const 0
  toMention = T.pack . show
  fromMention dis = MaybeT . pure . readMaybe . T.unpack

instance Mentionable T.Text where
  getId = const 0
  toMention = id
  fromMention dis = MaybeT . pure . Just

instance Mentionable Double where
  getId = const 0
  toMention = T.pack . show
  fromMention dis = MaybeT . pure . readMaybe . T.unpack

-- TODO: add a Mentionable instance for Roles and Emojis (or whatever they're called in the lib)

-- | Get snowflake from things of the form <c!FLAKE> 
-- where c is a char, and FLAKE is a Word64
parseMentionedFlake :: Char -> T.Text -> Snowflake
parseMentionedFlake c =
    unpackMaybeSnowflake 
  . (>>= readMaybe . T.unpack)
  . (>>= T.stripPrefix (T.pack $ '<':c:['!']))
  . T.stripSuffix (T.pack ">")

parseUserFlake :: T.Text -> Snowflake
parseUserFlake = parseMentionedFlake '@'

parseChannelFlake :: T.Text -> Snowflake
parseChannelFlake = parseMentionedFlake '#'
