{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Reader
import Polysemy.Resource
import qualified Colog as C
import qualified Colog.Polysemy as CP
import Data.Random.Distribution
import Data.Random.Distribution.Categorical
import Polysemy.RandomFu
import Discord
import GitHash

import Data.DiscordBot
import Data.Command

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

main :: IO ()
main = do
	dis <- loginRestGateway . Auth =<< getBotToken

	(runM
		.@ runResourceInIO)
		. runRandomIO
		. runReader dis
		. CP.runLogAction @IO C.richMessageAction
		. runGatewaySendableOutput
		. runSomeRequestOutput
		. runEventInput
		. reinterpretCommandInput
		. reinterpretDiscordBot
		. logDiscordbot
		$ (initBot >> bot) `finally` exit

exit :: Members '[Reader DiscordConnection, Lift IO] r => Sem r ()
exit = ask @DiscordConnection >>= sendM . stopDiscord

initBot :: Member DiscordBot r => Sem r ()
initBot = updateBotStatus . UpdateBotStatusOpts $ UpdateStatusOpts
	{ updateStatusSince = Nothing
	, updateStatusGame = Just Activity
		{ activityName = "with freer monads"
		, activityType = ActivityTypeGame
		, activityUrl = Nothing
		}
	, updateStatusNewStatus = UpdateStatusOnline
	, updateStatusAFK = False
	}

bot :: Members [DiscordBot, RandomFu] r => Sem r ()
bot = getCommand >>= \case
	BotCmd channel cmd -> case cmd of
		InvalidCmd e -> sendMessage channel e >> bot
		PingPong -> sendMessage channel "pong!" >> bot
		RandomChoice as -> do
			let choiceDist = fromWeightedList as

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
