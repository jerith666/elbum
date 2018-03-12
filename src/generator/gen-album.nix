{ mkDerivation, aeson, base, bytestring, directory_1_3_2_0, elm-bridge
, filepath, friday, friday-juicypixels, JuicyPixels, regex-compat
, stdenv
}:
mkDerivation {
  pname = "elbum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory_1_3_2_0 elm-bridge filepath friday
    friday-juicypixels JuicyPixels regex-compat
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = stdenv.lib.licenses.gpl3;
}
