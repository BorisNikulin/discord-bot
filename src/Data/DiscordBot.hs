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

logMsg:: Member (CP.Log C.Message) r => C.Severity -> T.Text -> Sem r ()
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

logDiscordbot :: Members '[CP.Log C.Message, DiscordBot] r => Sem r a -> Sem r a
logDiscordbot = intercept \case
	GetCommand -> logMsg Info "GetCommand" >> getCommand
	SendMessage channel txt -> flip unLogAction (channel, txt) do
		(T.pack . show >$< logInfo) >*< logInfo
		>> sendMessage channel txt
		where
			logInfo = C.LogAction $ logMsg C.Error
	UpdateBotStatus opts -> logMsg Info "UpdateBotStatus" >> updateBotStatus opts

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
