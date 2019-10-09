{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Reader
import Polysemy.Resource
import Colog hiding (Message, logMsg, logInfo)
import qualified Colog as C
import qualified Colog.Polysemy as CP
import Data.Random.Distribution
import Data.Random.Distribution.Categorical
import Polysemy.RandomFu
import qualified Discord as D
import GitHash

import Data.DiscordBot
import Data.Command

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

main :: IO ()
main = do
	dis <- D.loginRestGateway . D.Auth =<< getBotToken

	runM
		. resourceToIO
		. runRandomIO
		. runReader dis
		. CP.runLogAction @IO C.richMessageAction
		. runGatewaySendableOutput
		. logGatewaySendableOutput
		. runSomeRequestOutput
		. logSomeRequestOutput
		. runEventInput
		. logDiscordEventInput
		. reinterpretCommandInput
		. reinterpretDiscordBot
		. logDiscordBot
		$ (initBot >> bot) `finally` exit

exit :: Members '[Reader DiscordConnection, Embed IO] r => Sem r ()
exit = ask @DiscordConnection >>= embed . D.stopDiscord

initBot :: Member DiscordBot r => Sem r ()
initBot = updateBotStatus . UpdateBotStatusOpts $ D.UpdateStatusOpts
	{ D.updateStatusSince = Nothing
	, D.updateStatusGame = Just D.Activity
		{ D.activityName = "with freer monads"
		, D.activityType = D.ActivityTypeGame
		, D.activityUrl = Nothing
		}
	, D.updateStatusNewStatus = D.UpdateStatusOnline
	, D.updateStatusAFK = False
	}

bot :: Members [DiscordBot, CP.Log C.Message, RandomFu] r => Sem r ()
bot = getCommand >>= \case
	BotCmd channel cmd -> case cmd of
		InvalidCmd e -> sendMessage channel e >> bot
		PingPong -> sendMessage channel "pong!" >> bot
		RandomChoice as -> do
			let choiceDist = fromWeightedList as

			logMsg Debug $ "Distribution: " <> T.pack (show choiceDist)

			if numEvents choiceDist <= 0
				then sendMessage channel "```error: the sum of all weights must be effectively greater than zero```"
				else sampleRVar (rvar choiceDist) >>= sendMessage channel
			bot
		Version -> sendMessage channel (
				"``` version: "
				<> T.pack do giHash gitInfo
				<> " ("
				<> T.pack do giCommitDate gitInfo
				<> ")```"
			) >> bot
		Stop -> return ()

getBotToken :: IO T.Text
getBotToken = T.strip <$> TIO.readFile "./secrets/bot-token"
