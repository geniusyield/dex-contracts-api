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

## Spinning up server

Endpoints made available by server are specified [here](./web/swagger/api.yaml).

1. Make sure your environment is configured properly, consult ["How to build?"](https://atlas-app.io/how-to-build/) section of Atlas documentation for it.
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
    ```
3. Run the server with command `cabal run geniusyield-server -- serve -c my-config.yaml`. Run `cabal run geniusyield-server -- -h` for help 😉.
4. Test if server is running successfully by calling, say, `/settings` endpoint. Example `curl` request: `curl -H 'api-key: YOUR_SECRET_KEY' -X GET http://localhost:8082/v0/settings | jq`, assuming port was specified as `8082`. On success, it should return something akin to:
    ```json
    {
      "network": "mainnet",
      "version": "0.1.0",
      "revision": "c2f8db2bc82a13c850c3b0088a1ce089bb1065b7",
      "backend": "mmb"
    }

## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
