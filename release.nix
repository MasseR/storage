{ nixpkgs ? import <nixpkgs> {} }:

let
  masse-prelude-src = nixpkgs.fetchFromGitHub {
    owner = "MasseR";
    repo = "masse-prelude";
    rev = "69c2d9ec41e5b91cdf26eafada35f144359de8f9";
    sha256 = "1f2nrmhi4x6lvlgb96k8gykz30z9z3qf1z7w4ybb3j2ljj2gb9nq";
  };
  hp = nixpkgs.haskellPackages.extend (self: super: rec {
    masse-prelude = self.callPackage masse-prelude-src {};
    merkle = self.callPackage ./merkle {};
    storage-core = self.callPackage ./storage-core {};
    storage-server = self.callPackage ./storage-server {};
    storage-api = self.callPackage ./storage-api {};
    storage-client = self.callPackage ./storage-client {};
    orphans = self.callPackage ./orphans {};
  });

in

rec {
  inherit (hp) merkle storage-core storage-server storage-api storage-client;
  shell = hp.shellFor {
    packages = h: with h; [merkle storage-core storage-server storage-api storage-client orphans];
    buildInputs = with nixpkgs; [
      entr
      ghcid
      hp.cabal-install
      hp.hasktags
      stylish-haskell
      hlint
    ];
  };
}
