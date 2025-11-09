{
  description = "Plutus Voting Smart Contract";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    plutus.url = "github:input-output-hk/plutus-apps";
  };

  outputs = { self, nixpkgs, haskellNix, iohk-nix, plutus }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskellNix.overlay iohk-nix.overlay ];
        inherit (haskellNix) config;
      };
    in
    {
      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          haskell = pkgs.haskell-nix.project {
            src = ./.;
            compiler-nix-name = "ghc8107";
            shell = {
              tools = {
                cabal = { };
                haskell-language-server = { };
                hlint = { };
                fourmolu = { };
                haskell-debug-adapter = { };
              };
              buildInputs = with pkgs; [
                z3
                python3
                nodejs
                pkgconfig
                gmp
                libsodium
                secp256k1
              ];
            };
          };
        in
        {
          default = haskell.shellFor {
            packages = p: with p; [ voting-contract ];
          };
        });

      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          haskell = pkgs.haskell-nix.project {
            src = ./.;
            compiler-nix-name = "ghc8107";
          };
        in
        {
          voting-contract = haskell.voting-contract.components.library;
          default = self.packages.${system}.voting-contract;
        });
    };
}