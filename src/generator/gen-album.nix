{ mkDerivation, aeson, async, base, bytestring, elm-bridge
, filepath, JuicyPixels, regex-compat, parallel, extra, safe
, lib
}:
mkDerivation {
  pname = "elbum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring elm-bridge filepath
    JuicyPixels regex-compat parallel extra safe
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = lib.licenses.gpl3;
}
