module Data.Command where

import Data.Text (Text)
import Discord

data BotCmd = BotCmd
	{ botCmdChannel :: ChannelId
	, botCmdCmd :: Cmd
	} deriving Show

data Cmd
	= InvalidCmd Text
	| PingPong
	| RandomChoice [(Float, Text)]
	| Stop
	| None
	deriving Show
