module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Reader
import Polysemy.Resource
import qualified Colog as C
import qualified Colog.Polysemy as CP
import Discord
import System.Random

import Data.DiscordBot
import Data.Command

main :: IO ()
main = do
	dis <- loginRestGateway . Auth =<< getBotToken

	(runM
		.@ runResourceInIO)
		. runReader dis
		. CP.runLogAction @IO C.richMessageAction
		. runGatewaySendableOutput
		. runSomeRequestOutput
		. runEventInput
		. reinterpretCommandInput
		. reinterpretDiscordBot
		. logDiscordbot
		$ (initBot >> bot) `finally` exit
	--where
		--logger = C.upgradeMessageAction C.defaultFieldMap $ C.cmapM C.fmtRichMessageDefault $ C.cmap C.fmtMessage C.logTextStdout

exit :: Members '[Reader DiscordConnection, Lift IO] r => Sem r ()
exit = ask @DiscordConnection >>= sendM . stopDiscord

initBot :: Member DiscordBot r => Sem r ()
initBot = updateBotStatus . UpdateBotStatusOpts $ UpdateStatusOpts
	{ updateStatusSince = Nothing
	, updateStatusGame = Just Activity
		{ activityName = "with free monads"
		, activityType = ActivityTypeGame
		, activityUrl = Nothing
		}
	, updateStatusNewStatus = UpdateStatusOnline
	, updateStatusAFK = False
	}

bot :: Members [DiscordBot, Lift IO] r => Sem r ()
bot = getCommand >>= \case
	InvalidCommand channel e -> sendMessage channel e >> bot
	PingPong channel -> sendMessage channel "pong!" >> bot
	RandomChoice channel as -> do
		rng <- sendM $ randomRIO (0, length as - 1)
		sendM $ putStrLn "log with putStrLn test"
		sendMessage channel (as !! rng)
		bot
	None -> bot
	Stop -> return ()

getBotToken :: IO T.Text
getBotToken = T.strip <$> TIO.readFile "./secrets/bot-token"
