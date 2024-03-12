# GeniusYield DEX

This repository houses on-chain smart contracts, Haskell off-chain interaction logic and server endpoints enabling users to easily interact with DEX in language of their choice. 

## Table of Contents

- [Structure of repository](#structure-of-repository)
- [Spinning up server](#spinning-up-server)
- [Contributing](#contributing)
- [License](#license)

## Structure of repository

- [`geniusyield-dex-api`](./geniusyield-dex-api/) provides off-chain code to interact with our DEX.
- [`geniusyield-server-lib`](./geniusyield-server-lib/) serves endpoints using our off-chain code to easily interact with GeniusYield DEX in language of user's choice.
- [`geniusyield-orderbot`](./geniusyield-orderbot/) cater to additional requirements such as building up of an order-book, receiving price feed, etc.

## Spinning up server

1. Make sure your environment is configured properly, consult ["How to build?"](https://atlas-app.io/how-to-build/) section of Atlas documentation for it.
2. Prepare a configuration, which can be stored either in file or in `SERVER_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Blockchain provider used by Atlas, our off-chain transaction building tool.
     # Supported values of `coreProvider` represented as JSON for brevity:
     # Local node in combination of Kupo, `{ socketPath: string, kupoUrl: string }`
     # Maestro, `{ maestroToken: string, turboSubmit: boolean }`
     # Blockfrost, `{ blockfrostKey: string }`
     # Note that Blockfrost is not recommended as some of the operations performed aren't optimal with it.
    coreProvider:  # This is a blockchain provider.
      maestroToken: YOUR_MAESTRO_TOKEN
      turboSubmit: false
     # Network id, only `mainnet` and `preprod` are supported for at the moment.
    networkId: mainnet
    logging:
      - type:
          tag: stderr
        severity: Debug
        verbosity: V2
     # Port to serve endpoints at.
    port: 8082
     # Maestro API key (token) to access information such as asset details given it's currency symbol and token name.
    maestroToken: YOUR_MAESTRO_TOKEN
     # API key to protect server endpoints with. It's value must be provided under `api-key` header of request.
    serverApiKey: YOUR_SECRET_KEY
    ```
3. Run the server with command `cabal run geniusyield-server -- serve -c my-config.yaml`. Run `cabal run geniusyield-server -- -h` for help 😉.

## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
