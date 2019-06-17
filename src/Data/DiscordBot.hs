{-# LANGUAGE TemplateHaskell#-}

module Data.DiscordBot where

import Data.Aeson
import qualified Data.Text as T
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Discord

import Data.Command
import Text.CommandParser

data SomeRequest where
	SomeRequest :: (FromJSON a, Request (r a)) => r a -> SomeRequest

type DiscordConnection = (RestChan, Gateway, [ThreadIdType])

data DiscordBot m a where
	GetCommand :: DiscordBot m Command
	SendMessage :: ChannelId -> T.Text -> DiscordBot m ()

makeSem ''DiscordBot

reinterpretDiscordBot :: Sem (DiscordBot ': r) a -> Sem (Input Command ': Output SomeRequest ': r) a
reinterpretDiscordBot = reinterpret2 \case
	GetCommand -> input @Command
	SendMessage channel txt -> output . SomeRequest $ CreateMessage channel txt

reinterpretCommandInput :: Sem (Input Command ': r) a -> Sem (Input Event ': r) a
reinterpretCommandInput = reinterpret \case
	Input -> input >>= \case
		MessageCreate m
			| not $ fromBot m -> return $ case parseCommand $ messageText m of
				InvalidCommand _ (Just e) -> InvalidCommand (messageChannel m) e
				InvalidCommand _ Nothing  -> InvalidCommand (messageChannel m) "invalid command"
				PingPong _                -> PingPong $ messageChannel m
				Stop                      -> Stop
				None                      -> None
		_ -> return None

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

interpretEventInput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Input Event ': r) a -> Sem r a
interpretEventInput = interpret \case
	Input -> ask @DiscordConnection >>= sendM . nextEvent >>= \case
		Right e -> return e
		_ -> error "TODO"

interpretSomeRequestOutput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Output SomeRequest ': r) a -> Sem r a
interpretSomeRequestOutput = interpret \case
	Output (SomeRequest r) -> ask @DiscordConnection >>= sendM . flip restCall r >> return ()
