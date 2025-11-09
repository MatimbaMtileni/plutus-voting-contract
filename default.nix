{ system ? builtins.currentSystem }:
let
  pkgs = import <nixpkgs> { inherit system; };
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  name = "voting-contract";
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      hlint
      fourmolu
    ]);
}