{ mkDerivation, aeson, async, base, bytestring, elm-bridge
, filepath, friday, friday-juicypixels, JuicyPixels, regex-compat, parallel
, stdenv
}:
mkDerivation {
  pname = "elbum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring elm-bridge filepath friday
    friday-juicypixels JuicyPixels regex-compat parallel
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = stdenv.lib.licenses.gpl3;
}
