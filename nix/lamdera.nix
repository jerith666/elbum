# from https://gist.githubusercontent.com/supermario/e34de4ee5b37c54041320be7dd252e6b/raw/333def480d6dba5bce7f65f03d608311118a961c/default.nix

{ stdenv
, lib
, fetchurl
, ncurses5
, gmp5
, zlib
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  name = "lamdera-${version}";

  version = "1.0.1";

  src = fetchurl {
    url = "https://static.lamdera.com/bin/linux/lamdera-v1.0.1";
    sha256 = "sha256:15dee9df5d4e71b07a65fbd89d0f7dcd8c3e7ba05fe2b0e7a30d29bbd1239d9f";

  };

  unpackPhase = ":";

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    ncurses5
    gmp5
    zlib
  ];

  sourceRoot = ".";

  installPhase = ''
    install -m755 -D $src $out/bin/lamdera
  '';

  meta = with lib; {
    homepage = "https://lamdera.com/";
    license = licenses.unfree;
    description = "Lamdera";
    platforms = [ "x86_64-linux" ];
  };
}
