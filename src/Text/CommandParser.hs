module Text.CommandParser where

import Data.Void
import Data.Char
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Command

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

prefix :: Parser Text
prefix = string "!"

stop :: Parser (Command' Maybe)
stop = Stop <$ string' "stop"

pingPong :: Parser (Command' Maybe)
pingPong = PingPong Nothing <$ string' "ping"

randomChoice :: Parser (Command' Maybe)
randomChoice = symbol "r" *> do
	RandomChoice Nothing . Just
		<$> sepBy1 (lexeme $ takeWhile1P Nothing (\c -> isAlphaNum c || isSpace c)) (symbol "|")

command :: Parser (Command' Maybe)
command = prefix *> choice [stop, pingPong, randomChoice] <* eof <|> pure None

parseCommand :: Text -> Command' Maybe
parseCommand t = case runParser command "" t of
	Right cmd -> cmd
	Left e    -> InvalidCommand Nothing . Just. pack $ errorBundlePretty e
