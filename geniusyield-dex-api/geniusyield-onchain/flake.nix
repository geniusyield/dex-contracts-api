{
  description = "geniusyield-onchain";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
          ];
          inherit (haskellNix) config;
        };
        onchain = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc925";
          shell = {
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server` etc
            tools = {
              cabal = "3.8.1.0";
              haskell-language-server = {
                version = "1.8.0.0";
                index-state = "2022-10-31T00:00:00Z";
              };
              hlint = "3.5";
            };
            # Non-Haskell shell tools go here
            buildInputs = with pkgs; [
              git
            ];
          };

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
        };
      in onchain.flake { });
}
