{ mkDerivation, aeson, base, binary, cereal, comonad, filepath
, genvalidity, genvalidity-text, lens, masse-prelude, merkle, pipes
, pipes-bytestring, pipes-group, QuickCheck, safecopy, servant
, stdenv, unliftio
}:
mkDerivation {
  pname = "storage-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary cereal comonad filepath genvalidity
    genvalidity-text lens masse-prelude merkle pipes pipes-bytestring
    pipes-group QuickCheck safecopy servant unliftio
  ];
  license = stdenv.lib.licenses.mit;
}
