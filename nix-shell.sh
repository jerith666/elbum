#!/usr/bin/env bash

#fails building friday-juicypixels, an old version
#nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ihaskell-juicypixels elm-bridge friday-juicypixels])"

#fails building some other upstream haskell package (ghc-parser):
#http://hydra.nixos.org/build/36519168
#https://github.com/gibiansky/IHaskell/pull/686
#nix-shell -I nixpkgs=/home/matt/git/nixos/nixpkgs -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ihaskell-juicypixels elm-bridge])"

nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [friday-juicypixels elm-bridge_0_4_1 friday directory_1_3_1_1 regex-compat])"

#cabal install friday-juicypixels
