# Discord Bot in Haskell with Freer Monads

## Dependencies
* stack

## Setup
Create a discord bot using available guides.

`git clone` the repo.

Create a secrets folder at the top level of the repo.

Create a `bot-token` file in the secrets folder.

Paste discord bot token into the file such that
the file contains only the token.

`stack build` to build.

`stack exec discord-bot-exe` to run the bot.

