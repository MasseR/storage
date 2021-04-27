{ mkDerivation, aeson, base, ekg-core, genvalidity
, genvalidity-hspec, genvalidity-hspec-aeson, genvalidity-text
, hspec, hspec-golden-aeson, masse-prelude, merkle, pipes
, QuickCheck, servant, servant-pipes, stdenv
}:
mkDerivation {
  pname = "storage-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base ekg-core genvalidity genvalidity-text masse-prelude
    merkle pipes QuickCheck servant servant-pipes
  ];
  testHaskellDepends = [
    base genvalidity-hspec genvalidity-hspec-aeson hspec
    hspec-golden-aeson masse-prelude QuickCheck
  ];
  license = stdenv.lib.licenses.mit;
}
