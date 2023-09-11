{ nixpkgs ? import <nixpkgs> { } }:
nixpkgs.pkgs.haskellPackages.callPackage ./gen-album.nix { }
