{-# LANGUAGE TemplateHaskell#-}

module Data.DiscordBot where

import Data.Char
import qualified Data.Text as T
import Data.Aeson
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Discord

data SomeRequest where
	SomeRequest :: (FromJSON a, Request (r a)) => r a -> SomeRequest

type DiscordConnection = (RestChan, Gateway, [ThreadIdType])

data Command
	= PingPong ChannelId
	| Stop
	| None

data DiscordBot m a where
	GetCommand :: DiscordBot m Command
	Pong :: ChannelId -> DiscordBot m ()

makeSem ''DiscordBot

reinterpretDiscordBot :: Sem (DiscordBot ': r) a -> Sem (Input Command ': Output SomeRequest ': r) a
reinterpretDiscordBot = reinterpret2 \case
	GetCommand -> input @Command
	Pong channel -> output . SomeRequest $ CreateMessage channel "pong"

reinterpretCommandInput :: Sem (Input Command ': r) a -> Sem (Input Event ': r) a
reinterpretCommandInput = reinterpret \case
	Input -> input >>= \case
		MessageCreate m
			| not $ fromBot m -> return case messageText m of
				txt
					| isPing txt -> PingPong $ messageChannel m
					| isStop txt -> Stop
					| otherwise  -> None
		_ -> return None

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower

isStop :: T.Text -> Bool
isStop = T.isPrefixOf "stop" . T.map toLower

interpretEventInput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Input Event ': r) a -> Sem r a
interpretEventInput = interpret \case
	Input -> ask @DiscordConnection >>= sendM . nextEvent >>= \case
		Right e -> return e
		_ -> error "TODO"

interpretSomeRequestOutput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Output SomeRequest ': r) a -> Sem r a
interpretSomeRequestOutput = interpret \case
	Output (SomeRequest r) -> ask @DiscordConnection >>= sendM . flip restCall r >> return ()
