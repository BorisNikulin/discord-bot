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

stop :: Parser Cmd
stop = Stop <$ string' "stop"

pingPong :: Parser Cmd
pingPong = PingPong <$ string' "ping"

randomChoice :: Parser Cmd
randomChoice = symbol "r" *> do
	RandomChoice . zip (repeat 1) -- TODO actually optionally parse weights
		<$> sepBy1 (lexeme $ takeWhile1P Nothing (\c -> isAlphaNum c || isSpace c)) (symbol "|")

command :: Parser Cmd
command = prefix *> choice [stop, pingPong, randomChoice] <* eof <|> pure None

parseCommand :: Text -> Cmd
parseCommand t = case runParser command "" t of
	Right cmd -> cmd
	Left e    -> InvalidCmd . pack $ errorBundlePretty e
