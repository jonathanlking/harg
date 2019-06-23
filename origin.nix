{ mkDerivation, aeson, barbies, base, higgledy, hpack, lib
, optparse-applicative, stdenv, text, validation
}:
mkDerivation {
  pname = "origin";
  version = "0.1.0.0";
  src = lib.sourceByRegex ./. [
    "app(.*)?"
    "src(.*)?"
    "test(.*)?"
    "origin.cabal"
    "package.yaml"
  ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson barbies base higgledy optparse-applicative text validation
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson barbies base higgledy optparse-applicative text validation
  ];
  testHaskellDepends = [
    aeson barbies base higgledy optparse-applicative text validation
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/alexpeits/origin#readme";
  license = stdenv.lib.licenses.bsd3;
}