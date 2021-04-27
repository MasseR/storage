{ mkDerivation, aeson, base, base16-bytestring, binary, cereal
, comonad, cryptonite, genvalidity, genvalidity-bytestring
, genvalidity-hspec, genvalidity-hspec-aeson, genvalidity-text
, hspec, http-api-data, lens, masse-prelude, memory
, recursion-schemes, safecopy, stdenv
}:
mkDerivation {
  pname = "merkle";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring binary cereal comonad cryptonite
    genvalidity genvalidity-bytestring genvalidity-text http-api-data
    lens masse-prelude memory recursion-schemes safecopy
  ];
  testHaskellDepends = [
    aeson base base16-bytestring binary cereal comonad cryptonite
    genvalidity genvalidity-bytestring genvalidity-hspec
    genvalidity-hspec-aeson genvalidity-text hspec http-api-data lens
    masse-prelude memory recursion-schemes safecopy
  ];
  license = stdenv.lib.licenses.mit;
}
