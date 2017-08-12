{ mkDerivation, aeson, array, base, base16-bytestring
, base64-bytestring, binary, bytestring, cborg, cereal
, cereal-vector, containers, criterion, deepseq, directory
, fetchgit, filepath, ghc-prim, half, hashable, old-locale, pretty
, primitive, QuickCheck, quickcheck-instances, scientific, stdenv
, store, tar, tasty, tasty-hunit, tasty-quickcheck, text, time
, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "serialise";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/cborg";
    sha256 = "05yihawj6g35asihj07kcfvswm67837pi1imy7hqfsdxjxdsnljb";
    rev = "60199f415e056d70f8f4426d6d3c6cd41e434c83";
  };
  postUnpack = "sourceRoot+=/serialise; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base bytestring cborg containers ghc-prim half hashable
    old-locale primitive text time unordered-containers vector
  ];
  testHaskellDepends = [
    aeson array base base16-bytestring base64-bytestring binary
    bytestring cborg containers deepseq directory filepath ghc-prim
    half hashable QuickCheck quickcheck-instances scientific tasty
    tasty-hunit tasty-quickcheck text time unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson array base binary bytestring cborg cereal cereal-vector
    containers criterion deepseq directory filepath ghc-prim half
    old-locale pretty store tar text time vector zlib
  ];
  homepage = "https://github.com/well-typed/cborg";
  description = "A binary serialisation library for Haskell values";
  license = stdenv.lib.licenses.bsd3;
}
