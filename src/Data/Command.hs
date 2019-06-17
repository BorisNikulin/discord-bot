module Data.Command where

import Data.Functor.Identity
import Discord

type family HKD f a where
	HKD Identity a = a
	HKD f        a = f a

data Command' f
	= InvalidCommand (HKD f ChannelId) (HKD f String)
	| PingPong (HKD f ChannelId)
	| Stop
	| None

type Command = Command' Identity
