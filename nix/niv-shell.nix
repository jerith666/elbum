{ sources ? import ./sources.nix }:

let
  nixpkgs = import sources.nixpkgs { };
in

nixpkgs.pkgs.stdenv.mkDerivation {
  name = "niv-env-0";
  buildInputs = with nixpkgs.pkgs;
    [
      niv
      nix
      nvd
    ];
}
