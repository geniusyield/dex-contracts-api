# GeniusYield DEX

This repository houses on-chain smart contracts, Haskell off-chain interaction logic and server endpoints enabling users to easily interact with DEX in language of their choice. 

## Table of Contents

- [Structure of repository](#structure-of-repository)
- [Spinning up server](#spinning-up-server)
- [Contributing](#contributing)
- [License](#license)

## Structure of repository

- [`geniusyield-dex-api`](./geniusyield-dex-api/) provides off-chain code to interact with our DEX. See it's [`README.md`](./geniusyield-dex-api/README.md) for more information about it.
- [`geniusyield-server-lib`](./geniusyield-server-lib/) serves endpoints using our off-chain code to easily interact with GeniusYield DEX in language of user's choice.
- [`geniusyield-orderbot-lib`](./geniusyield-orderbot-lib/) cater to additional requirements such as building up of an order-book, receiving price feed, etc.

# Bot API Server

## Spinning up the api server using docker

The api server can be started using [docker-compose](https://github.com/geniusyield/dex-contracts-api/blob/main/docker-compose.yml). Simply clone the repository,
prepare a `.env` file with the necessary secrets and use the make targets from
the Makefile available in the repository.

One could spin up a bot api server using the following commands:

``` bash
# Clone the reposizoty:
git clone git@github.com:geniusyield/dex-contracts-api.git
cd dex-contracts-api
# Prepare the .env file with the secrets:
echo """CORE_MAESTRO_API_KEY=___REPLACE_ME___
MAESTRO_API_KEY=___REPLACE_ME___
SERVER_API_KEY=___REPLACE_ME___
SEED_PHRASE=[word_1, word_2, ... word_23, word_24]""" > .env
nano .env
# Stop any previously running services (cleaning up)
make stop
# Pull the most recent versions of the relevant docker image(s)
make pull
# Start the services
make start
sleep 60 # wait for the server start or alternatively
         # one could trail the logs with: `make logs`.
make test # send test request
```

The last line should result in a JSON output like the following document:

```json
{
  "network":"mainnet",
  "version":"0.1.0",
  "revision":"e9715955919566e465cbf247480977f46f8809d2",
  "backend":"mmb",
  "address":"addr1qx...w60mw",
  "stake_address":null,
  "collateral":null
}
```

This output means that the bot api backend metadata could be retrieved via the `settings` endpoint and the server is up and running and it is ready to process requests.

The Trading Bot API could by using by Custom Trading Bots or Trading Strategy implementations run
using the [Trading Strategy Executor Framework](https://github.com/geniusyield/strategy-executor/tree/main) (Python SDK for the Genius Yield DEX).

## Building from source

Alternatively the software could be built from source code on the local workstation using
the Haskell toolchain.

For details please see the following section:

Make sure your environment is configured properly, consult ["How to build?"](https://atlas-app.io/getting-started/how-to-build) section of Atlas documentation for it.

## Swagger API documentation

Endpoints made available by server are specified [here](./web/swagger/api.yaml).

## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
