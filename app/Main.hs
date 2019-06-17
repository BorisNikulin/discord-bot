module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Reader
import Discord

import Data.DiscordBot
import Data.Command

main :: IO ()
main = do
	dis <- loginRestGateway . Auth =<< getBotToken

	runM
		. runReader dis
		. interpretSomeRequestOutput
		. interpretEventInput
		. reinterpretCommandInput
		$ reinterpretDiscordBot pingPong

	stopDiscord dis

pingPong :: Member DiscordBot r => Sem r ()
pingPong = getCommand >>= \case
	InvalidCommand channel e -> sendMessage channel (T.pack e) >> pingPong
	PingPong channel -> sendMessage channel "pong!" >> pingPong
	None -> pingPong
	Stop -> return ()

getBotToken :: IO T.Text
getBotToken = T.strip <$> TIO.readFile "./secrets/bot-token"
