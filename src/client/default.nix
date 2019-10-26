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

      gitignore = fetchurl {
        name = "gitignore";
        url = https://raw.githubusercontent.com/elm/core/2b9a88dfd78545d02e2d3caacb0a7326765cb309~/.gitignore;
        sha512 = "2yiv1fgjlwmi6x0865xm9cqjnd2xzqlywvkn8f1igqd56z5afh24bb0xs0k4xcfwvyvgsvnaqql6s252vg780sr5mxy7z3rnm5hbhkg";
      };

      prePatch = "pushd $ELM_HOME/*/packages/elm/core/*; cp -v ${gitignore} .gitignore";
      patches = lib.reverseList (
        map (p: fetchurl {
              url = "https://github.com/elm/core/pull/1018/commits/${p.c}.patch";
              sha512 = p.sha512;
            })
            [
              { c = "6eff09b584331242164cc9849e15378eb91074af"; sha512="304j6dp475srawybq6qlhlhczp6f0sn8xidl6pm7khb7zv9k7v73hqhmpzisywl1k79rsjwjn4dl6qj91gf0byp2ilj9b710spmgz33"; }
              { c = "2b9a88dfd78545d02e2d3caacb0a7326765cb309"; sha512="171v1jvrv6lsf3hngwcbj607kfvz6h2jv3ghgrizk2dnsfcx598nk2k08qpdzf3bh6w1igbnsmaxg86fv08zh82yq2cs1hvyv7h7wg4"; }
              { c = "d28daece5a2a6ca423b0d1b9a9dbe10b890fd1b1"; sha512="0ha5fhgwanm1g7xws7qcy0swnk5rz73k520gpwlysxs80m3cpmr5zkx0k9qbf1c7zkmdk988swapyhddwpam69dp5fhaww2r3j475mp"; }
              { c = "34221393af1591660142bd6f2a8cbc7ea0ea20fa"; sha512="22krsvafbim7lmf351pgvf65ffrfssa2c1vn90kgzwhfrbb3aaxgrxmjqnfa34m7ab2d9z2r942zap4z8lx4g7lkpss19ikcp1l8aw3"; }
              { c = "c86d7ddd06a5362aa7da4fe064021fe814d94ecd"; sha512="0gps6nxx28iickdzb1mbpwq27wamlyng1da5j54spm1j6gwqbnyl14429nasgndma87dslknkdfrigr3fndvsji3rks9s0q7zqsf0q4"; }
              { c = "5c0059b7b900a51547b7f310030d21d571e3ebc2"; sha512="3pzfb1hzva5m2g33vxjjhlf2nzbwxw8rwj1ljhvzmm9ha1cq9a05ilabpmld1dmzckq5w5gc3qfckhk39b29j51jvz3hahxxrgpjv1a"; }
              { c = "7a79b041a3701739aa3c4fbced4391c0290901f5"; sha512="0vnzdi1dxh9dlihz9aaxncax53r6ms6qswz791x1zxzzla6lv2hajz8kr00abfqyi4zyd0yf4gmqa08iva4bjr1pk2nyvky22bf8zbc"; }
              { c = "4ac638a7672f2ac7e15dc2967feac9936a3687a7"; sha512="3q99n6lcv8xk2jkfn7zhyz3lmxhivhpr3rwa8vbpinfdy2wr0c99nng7vbkzigb6z92bhr82wkd57d35ipql18k22zmnvxc0cjrvl4v"; }
              { c = "3e7e6fa77c5979ebcdeed170ff013f0bb0c78c4a"; sha512="1y6k7xpwdm4nia7wm8g283say3zvqii3axr8gvsljdp10dp1jh6ymxi5qfxq3bv8x1ivnb65i7ggb2a8npv4al2r4lmm9884dy4va9a"; }
              { c = "8fd32173883143780f7ef9f407b8d9f1fb50d5da"; sha512="1lc76xm707z18aqv3m15zn5dj0pfmyy19qiw6g2h4q4j62h4qlay1m8arwddg2l7y48axlm9f7kx03aa994gc2jq9lcajgxkd6k93fx"; }
              { c = "0f342fa07e525dfce39d29b75eca9c341ffc35ee"; sha512="20wj5f38f5ajmnqshpgfaz2vgf3nlc9ai9q9ymw5l7zbg2hzkajj8qjayqki4d0h2jkgsc52jhdfjfl0jcrhj0d58avcs4c9pmj07xr"; }
              { c = "5d82bca00922a7156eb7f3d7767b05ed212f4d6f"; sha512="3wfq9wvfn889jwy83zshfgr6w06jw4w9wqhz5f86ama6nz21ycn5ncwgz9wwf9vcdm88pa31fkywq3dplkc9j2l5axaq0f1y3njvglf"; }
            ]);
      postPatch = "popd";

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p $out/share/doc
        cp -iv ${albumTypes}/Album.elm .;
        ${lib.concatStrings (map (module: ''
          elm make ${elmfile module} --output $out/${module}.js --docs $out/share/doc/${module}.json --optimize
        '') targets)}
      '';

      installPhase = ''
        mv -iv $out/Main.js $out/elbum.js;
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
  targets = [ "Main" ];
}
