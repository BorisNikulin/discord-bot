module Data.Command where

import Data.Text (Text)
import Discord

data BotCmd = BotCmd
	{ botCmdChannel :: ChannelId
	, botCmdCmd :: Cmd
	}

data Cmd
	= InvalidCmd Text
	| PingPong
	| RandomChoice [(Int, Text)]
	| Stop
	| None
