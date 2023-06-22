{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc927" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./gen-album.nix { }
