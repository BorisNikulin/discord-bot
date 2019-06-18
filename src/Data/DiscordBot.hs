{-# LANGUAGE TemplateHaskell#-}

module Data.DiscordBot where

import Data.Semigroup ((<>))
import Data.Aeson
import qualified Data.Text as T
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Colog hiding (Message, logMsg, logInfo)
import qualified Colog.Core as C
import qualified Colog.Message as C
import qualified Colog.Polysemy as CP
import Discord
import GHC.Stack

import Data.Command
import Text.CommandParser

logMsg:: Member (CP.Log C.Message) r => C.Severity -> T.Text -> Sem r ()
logMsg s m = withFrozenCallStack (CP.log $ Msg s callStack m)

data SomeRequest where
	SomeRequest :: (FromJSON a, Request (r a)) => r a -> SomeRequest

type DiscordConnection = (RestChan, Gateway, [ThreadIdType])

data UpdateBotStatusOpts = UpdateBotStatusOpts UpdateStatusOpts | UpdateBotStatusVoiceOpts UpdateStatusVoiceOpts

data DiscordBot m a where
	GetCommand :: DiscordBot m Command
	SendMessage :: ChannelId -> T.Text -> DiscordBot m ()
	UpdateBotStatus :: UpdateBotStatusOpts -> DiscordBot m ()

makeSem ''DiscordBot

logDiscordbot :: Members '[CP.Log C.Message, DiscordBot] r => Sem r a -> Sem r a
logDiscordbot = intercept \case
	GetCommand -> logMsg Info "GetCommand" >> getCommand
	SendMessage channel txt -> flip unLogAction (channel, txt) do
		(T.pack . show >$< logInfo) >*< logInfo
		>> sendMessage channel txt
		where
			logInfo = C.LogAction $ logMsg C.Error
	UpdateBotStatus opts -> logMsg Info "UpdateBotStatus" >> updateBotStatus opts

reinterpretDiscordBot :: Sem (DiscordBot ': r) a -> Sem (Input Command ': Output SomeRequest ': Output GatewaySendable ': r) a
reinterpretDiscordBot = reinterpret3 \case
	GetCommand -> input @Command
	SendMessage channel txt -> output . SomeRequest $ CreateMessage channel txt
	UpdateBotStatus opts -> case opts of
		UpdateBotStatusOpts o -> output $ UpdateStatus o
		UpdateBotStatusVoiceOpts o -> output $ UpdateStatusVoice o

reinterpretCommandInput :: Sem (Input Command ': r) a -> Sem (Input Event ': r) a
reinterpretCommandInput = reinterpret \case
	Input -> input >>= \case
		MessageCreate m
			| not $ fromBot m -> return let channel = messageChannel m
				in case parseCommand $ messageText m of
					InvalidCommand _ (Just e) -> InvalidCommand channel ("```" <> e <> "```")
					InvalidCommand _ Nothing  -> InvalidCommand channel "invalid command"
					PingPong _                -> PingPong channel
					RandomChoice _ (Just as)  -> RandomChoice channel as
					RandomChoice _ Nothing    -> InvalidCommand channel "invalid command: random choice needs elements"
					Stop                      -> Stop
					None                      -> None
		_ -> return None

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

runEventInput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Input Event ': r) a -> Sem r a
runEventInput = interpret \case
	Input -> ask @DiscordConnection >>= sendM . nextEvent >>= \case
		Right e -> return e
		_ -> error "TODO"

runSomeRequestOutput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Output SomeRequest ': r) a -> Sem r a
runSomeRequestOutput = interpret \case
	Output (SomeRequest r) -> ask @DiscordConnection >>= sendM . flip restCall r >> return ()

runGatewaySendableOutput :: Members '[Reader DiscordConnection, Lift IO] r => Sem (Output GatewaySendable ': r) a -> Sem r a
runGatewaySendableOutput = interpret \case
	Output s -> ask @DiscordConnection >>= sendM . flip sendCommand s >> return ()
