{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8106" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./gen-album.nix { }