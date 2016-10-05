#!/usr/bin/env bash

#fails building friday-juicypixels, an old version
#nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ihaskell-juicypixels elm-bridge friday-juicypixels])"

#fails building some other upstream haskell package (ghc-parser):
#http://hydra.nixos.org/build/36519168
#nix-shell -I nixpkgs=/home/matt/git/nixos/nixpkgs -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ihaskell-juicypixels elm-bridge])"

nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ihaskell-juicypixels elm-bridge friday])"

cabal install friday-juicypixels
