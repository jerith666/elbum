with import <nixpkgs> {};

let
  client = import src/client/default.nix {};
  generator = import src/generator/default.nix {};

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