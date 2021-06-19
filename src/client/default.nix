{ config ? {}
, nixpkgs ? import <nixpkgs> config }:

with nixpkgs;

let
  albumTypes = import ./album-types-gen.nix { inherit nixpkgs; };

  mkDerivation =
    { srcs ? ./nix/elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    }:
    stdenv.mkDerivation rec {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      postUnpack = (elmPackages.fetchElmDeps {
        elmVersion = "0.19.1";
        elmPackages = import ./nix/elm-srcs.nix;
        registryDat = ./nix/registry.dat;
      });

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p $out/share/doc
        cp -iv ${albumTypes}/Album.elm src;
        ${lib.concatStrings (map (module: ''
          elm make ${elmfile module} --output $out/${module}.js --docs $out/share/doc/${module}.json --optimize
        '') targets)}
      '';

      installPhase = ''
        mv -iv $out/src/Main.js $out/elbum.js;
        cp -iv index.html $out;
        cp -iv .htaccess $out;
      '';
    };
in mkDerivation {
  name = "jerith666-elbum-0.1.0";
  srcs = ./elm-srcs.nix;
  src = lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      (type == "regular" && (
        pkgs.lib.hasSuffix ".elm" path ||
        pkgs.lib.hasSuffix "elm.json" path ||
        pkgs.lib.hasSuffix "index.html" path ||
        pkgs.lib.hasSuffix ".htaccess" path
      )) ||
      (type == "directory" && (
        pkgs.lib.hasSuffix "vendor" path ||
        pkgs.lib.hasSuffix "elm-route-url" path ||
        pkgs.lib.hasSuffix "touch-events" path ||
        pkgs.lib.hasSuffix "src" path ||
        pkgs.lib.hasSuffix "Utils" path
      ));
  };
  srcdir = ".";
  targets = [ "src/Main" ];
}
