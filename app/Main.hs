module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Reader
import Discord

import Data.DiscordBot

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
	PingPong channel -> pong channel >> pingPong
	None -> pingPong
	Stop -> return ()

getBotToken :: IO T.Text
getBotToken = T.strip <$> TIO.readFile "./secrets/bot-token"
