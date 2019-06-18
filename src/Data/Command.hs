module Data.Command where

import Data.Functor.Identity
import Data.Text (Text)
import Discord

type family HKD f a where
	HKD Identity a = a
	HKD f        a = f a

data Command' f
	= InvalidCommand (HKD f ChannelId) (HKD f Text)
	| PingPong (HKD f ChannelId)
	| RandomChoice (HKD f ChannelId) (HKD f [Text])
	| Stop
	| None

type Command = Command' Identity
