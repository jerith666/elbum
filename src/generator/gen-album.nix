{ mkDerivation, aeson, async, hip, base, bytestring, elm-bridge
, filepath, friday, friday-juicypixels, JuicyPixels-extra, regex-compat, parallel, extra, safe
, lib
}:
mkDerivation {
  pname = "elbum";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async hip base bytestring elm-bridge filepath friday
    friday-juicypixels JuicyPixels-extra regex-compat parallel extra safe
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "a web photo album generator";
  license = lib.licenses.gpl3;
}
