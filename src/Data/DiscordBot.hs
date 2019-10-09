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
import qualified Discord as D
import GHC.Stack

import Data.Command
import Text.CommandParser

logMsg:: (HasCallStack, Member (CP.Log C.Message) r) => C.Severity -> T.Text -> Sem r ()
logMsg s m = withFrozenCallStack (CP.log $ Msg s callStack m)

data SomeRequest where
	SomeRequest :: (FromJSON a, D.Request (r a)) => r a -> SomeRequest

type DiscordConnection = (D.RestChan, D.Gateway, [D.ThreadIdType])

data UpdateBotStatusOpts
	= UpdateBotStatusOpts D.UpdateStatusOpts
	| UpdateBotStatusVoiceOpts D.UpdateStatusVoiceOpts

data DiscordBot m a where
	GetCommand :: DiscordBot m BotCmd
	SendMessage :: D.ChannelId -> T.Text -> DiscordBot m ()
	UpdateBotStatus :: UpdateBotStatusOpts -> DiscordBot m ()

makeSem ''DiscordBot

logDiscordBot :: Members '[CP.Log C.Message, DiscordBot] r => Sem r a -> Sem r a
logDiscordBot = intercept \case
	GetCommand -> do
		logMsg Info "Waiting for command"
		cmd <- getCommand
		logMsg Info $ "Got command from channel " <> T.pack (show $ botCmdChannel cmd)
		logMsg Info $ "Got command: " <> T.pack (show $ botCmdCmd cmd)
		return cmd
	SendMessage channel txt -> do
		logMsg Info $ "Replying to channel " <> T.pack (show channel)
		logMsg Info $ "Replying with message: " <> txt
		sendMessage channel txt
	UpdateBotStatus opts -> logMsg Info "Updating bot status" >> updateBotStatus opts

logDiscordEventInput
	:: Members
		'[ CP.Log C.Message
		,  Input D.Event
		] r
		=> Sem r a
		-> Sem r a
logDiscordEventInput = intercept \case
	(Input :: Input D.Event r a) -> do
		discordInput <- input
		logMsg Debug $ "Discord event: " <> T.pack (show discordInput)
		return discordInput

logSomeRequestOutput
	:: Members
		'[ CP.Log C.Message
		,  Output SomeRequest
		] r
		=> Sem r a
		-> Sem r a
logSomeRequestOutput = intercept \case
	(Output sr@(SomeRequest _) :: Output SomeRequest r a) -> do
		let msg = T.pack "Sending some request to discord"
		logMsg Debug $ msg
		output sr

reinterpretDiscordBot :: Sem (DiscordBot ': r) a -> Sem (Input BotCmd ': Output SomeRequest ': Output D.GatewaySendable ': r) a
reinterpretDiscordBot = reinterpret3 \case
	GetCommand -> input @BotCmd
	SendMessage channel txt -> output . SomeRequest $ D.CreateMessage channel txt
	UpdateBotStatus opts -> case opts of
		UpdateBotStatusOpts o -> output $ D.UpdateStatus o
		UpdateBotStatusVoiceOpts o -> output $ D.UpdateStatusVoice o

reinterpretCommandInput :: Sem (Input BotCmd ': r) a -> Sem (Input D.Event ': r) a
reinterpretCommandInput = reinterpret \case
	Input -> go where
		go = input >>= \case
				D.MessageCreate m
					| not $ fromBot m -> let channel = D.messageChannel m
						in case parseCommand $ D.messageText m of
							Just (InvalidCmd e) -> return . BotCmd channel $ InvalidCmd ("```" <> e <> "```")
							Just cmd            -> return $ BotCmd channel cmd
							Nothing             -> go
					| otherwise -> go
				_ -> go

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

runEventInput :: Members '[Reader DiscordConnection, Embed IO] r => Sem (Input D.Event ': r) a -> Sem r a
runEventInput = interpret \case
	Input -> ask @DiscordConnection >>= embed . D.nextEvent >>= \case
		Right e -> return e
		_ -> error "did not connect"

runSomeRequestOutput :: Members '[Reader DiscordConnection, Embed IO] r => Sem (Output SomeRequest ': r) a -> Sem r a
runSomeRequestOutput = interpret \case
	Output (SomeRequest r) -> ask @DiscordConnection >>= embed . flip D.restCall r >> return ()

runGatewaySendableOutput :: Members '[Reader DiscordConnection, Embed IO] r => Sem (Output D.GatewaySendable ': r) a -> Sem r a
runGatewaySendableOutput = interpret \case
	Output s -> ask @DiscordConnection >>= embed . flip D.sendCommand s >> return ()
