module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord

main :: IO (Either RestCallException Message)
main = do
	dis <- loginRest . Auth =<< getBotToken
	restCall dis $ CreateMessage 221469616789127168 "test post pls ignore"

getBotToken :: IO T.Text
getBotToken = T.strip <$> TIO.readFile "./secrets/bot-token"
