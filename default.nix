{ sources ? import nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs { };
in
with nixpkgs;

let
  client = import src/client/default.nix { inherit nixpkgs; };
  generator = import src/generator/default.nix { inherit nixpkgs; };

in

stdenv.mkDerivation {
  name = "elbum";
  src = ./src/elbum;
  buildInputs = [ client generator ];
  installPhase = ''
    mkdir -p $out/bin;
    cp -v elbum $out/bin
    substituteInPlace $out/bin/elbum \
      --replace @client@ ${client} \
      --replace @generator@ ${generator};
    substituteAllInPlace $out/bin/elbum;
  '';
}
