module Text.CommandParser where

import Data.Void
import Data.Char
import Data.Tuple
import Data.Text (Text)
import qualified Data.Text as T
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

choiceSep :: Char
choiceSep = '|'

choiceSubElement :: Parser Text
choiceSubElement = T.cons
	<$> label
		do "not '_', not '" <> [choiceSep] <> "'"
		do satisfy (\c -> c /= '_' && c /= choiceSep)
	<*> takeWhileP
		do Just $ "not ' ', not '" <> [choiceSep] <> "'"
		do \c -> (not $ isSpace c) && c /= choiceSep

choiceElement :: Parser Text
choiceElement = label "an element" $
	(T.concat .) . (:)
	<$> choiceSubElement
	<*> do many . try $
		T.append
		<$> takeWhileP Nothing isSpace
		<*> choiceSubElement

choiceWeight :: Parser Float
choiceWeight = label " a weight" do
	lexeme $ char '_' *> do try L.float <|> fmap (fromIntegral @Int @Float) L.decimal
	<|> pure 1

choiceOption :: Parser (Float, Text)
choiceOption = label "an element and an optional weight"
	$ (swap .) . (,) <$> lexeme choiceElement <*> choiceWeight

randomChoice :: Parser Cmd
randomChoice = symbol "r" *> do
	RandomChoice
		<$> sepBy1 choiceOption do symbol "|"

version :: Parser Cmd
version = Version <$ string "version"

command :: Parser Cmd
command = prefix *> cmd <* eof <|> pure None where
	cmd = choice
		[ stop
		, pingPong
		, randomChoice
		, version
		]

parseCommand :: Text -> Cmd
parseCommand t = case runParser command "" t of
	Right cmd -> cmd
	Left e    -> InvalidCmd . T.pack $ errorBundlePretty e
