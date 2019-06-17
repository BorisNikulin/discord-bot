module Text.CommandParser where

import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Command

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

prefix :: Parser Text
prefix = string "!"

pingPong :: Parser (Command' Maybe)
pingPong = PingPong Nothing <$ string' "ping"

stop :: Parser (Command' Maybe)
stop = Stop <$ string' "stop"

command :: Parser (Command' Maybe)
command =  prefix *> choice [pingPong, stop] <|> pure None

parseCommand :: Text -> Command' Maybe
parseCommand t = case runParser command "" t of
	Right cmd -> cmd
	Left e    -> InvalidCommand Nothing . Just $ errorBundlePretty e
