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

The `make test` call should result in a JSON output like the following document:

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

Thanks to the programming language agnostic RESTful API, any modern programming
language could be used to implement trading strategies and/or SOR, MMBs.

Intergration with the Genius Yield DEX has never been easier.

## Building locally from source using Docker

The easiest way to build the software is using docker.

Using the available Dockerfile the Haskell toolchain doesn't have to be installed locally:

``` bash
# Clone the reposizoty:
git clone git@github.com:geniusyield/dex-contracts-api.git
cd dex-contracts-api
# Build the docker image locally from source:
make build
```

If you would like to build the software from source locally on your workstation using the
Haskell toolchain, then please see the following section.

## Building locally from source using the Haskell Toolchain

Alternatively the software could be built from source code on the local workstation using
the Haskell toolchain.

For details please see the following section:

1. Make sure your environment is configured properly, consult ["How to build?"](https://atlas-app.io/getting-started/how-to-build) section of Atlas documentation for it.
2. Prepare a configuration, which can be stored either in file or in `SERVER_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Blockchain provider used by Atlas, our off-chain transaction building tool.
     # Supported values of `coreProvider` represented as JSON for brevity:
     # Local node in combination of Kupo, `{ socketPath: string, kupoUrl: string }`
     # Maestro, `{ maestroToken: string, turboSubmit: boolean }`
     # Blockfrost, `{ blockfrostKey: string }`
     # Note that Blockfrost is not recommended as some of the operations performed aren't optimal with it.
    coreProvider:
      maestroToken: YOUR_MAESTRO_TOKEN
      turboSubmit: false
     # Network id, only `mainnet` and `preprod` are supported for at the moment.
    networkId: mainnet
     # Logging configuration. It's an array to cater for potentially multiple scribes.
    logging:
      - type:
           # TODO: Possible values of `tag` are to be documented.
          tag: stderr
         # Possible values of `severity` are `Debug`, `Info`, `Warning` and `Error`.
        severity: Debug
         # Possible values of `verbosity` are `V0`, `V1`, `V2`, `V3` and `V4`. Consult https://hackage.haskell.org/package/katip-0.8.8.0/docs/Katip.html#t:Verbosity for more information about it.
        verbosity: V2
     # Port to serve endpoints at.
    port: 8082
     # Maestro API key (token) to access information such as asset details given it's currency symbol and token name.
    maestroToken: YOUR_MAESTRO_TOKEN
     # API key to protect server endpoints with. It's value must be provided under `api-key` header of request.
    serverApiKey: YOUR_SECRET_KEY
     # Optionally, wallet key details if one wants server to be able to sign transactions using this key.
    wallet:
      tag: mnemonicWallet
      contents:
        mnemonic:
          - health
          - unable
          - dog
          - lend
          - artefact
          - arctic
          - dinner
          - energy
          - silent
          - wealth
          - shock
          - safe
          - glad
          - mail
          - gas
          - flag
          - beauty
          - penalty
          - mixed
          - garbage
          - erupt
          - wonder
          - magnet
          - around
        # Account index.
        accIx: 0
        # Payment address index.
        addrIx: 0
     # Optionally, a stake address which is used to place orders at a mangled address, i.e., an address having payment component of the order validator but staking component from the given stake address. It has to bech32 encoded, with prefix "stake_test" for testnet and "stake" for mainnet.
    stakeAddress: stake1...
     # Optionally, one can specify collateral in the configuration to avoid sending it's information in the endpoints which require it.
    collateral: 15522d2518b36bdeb8e2e4829aff7ae9e5afbf74387d756543c5e955e83a9434#2
    ```
3. Run the server with command `cabal run geniusyield-server -- serve -c my-config.yaml`
4. Run `cabal run geniusyield-server -- -h` for help 😉
5. Test if server is running successfully by calling, say, `/settings` endpoint. Example `curl` request: `curl -H 'api-key: YOUR_SECRET_KEY' -X GET http://localhost:8082/v0/settings | jq`, assuming port was specified as `8082`. On success, it should return something akin to:

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

Alternatively you could also test using `make test`. This is sending a test GET request to the `settings` endpoint of the server running on the port `8082` of the `localhost`.

## Swagger API documentation

Endpoints made available by server are specified [here](./web/swagger/api.yaml).

## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
