{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.haskellPackages.callCabal2nix "bdd" ./. {}) ];
  buildInputs = with pkgs; [ cabal-install ];
}

  
