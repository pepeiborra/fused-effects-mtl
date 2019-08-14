let
  nixpkgs = fetchGit{
    name = "nixpkgs-08-13";
    url = https://github.com/NixOS/nixpkgs/;
    rev = "2fc0ff34a41662a7cbcb3fd7302cfa120e456f9b";
  };
in with import <nixpkgs>{};

let hsPkgs = haskellPackages.override {
    all-cabal-hashes =
      fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b134e7c22f70c7bfef92aaae5fa3bf8868ded6f8.tar.gz";
        sha256 = "111lnz2p75gb7cni324h5cphvig1ian5hg6rxbf167sjnr2sbkjw";
      };
    overrides = self: super: {
      fused-effects = self.callHackage "fused-effects" "0.5.0.0" {};
      };
     };

in runCommand "dummy" {
  buildInputs =
    let ghc = hsPkgs.ghcWithHoogle(p: [p.mtl p.fused-effects p.cpphs p.monad-control]); in
    [
      ghc
    ];
} ""
