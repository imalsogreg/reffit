{ mkDerivation, array, base, bytestring, containers, fetchgit
, ghc-prim, half, integer-gmp, primitive, stdenv, text
}:
mkDerivation {
  pname = "cborg";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/well-typed/cborg";
    sha256 = "05yihawj6g35asihj07kcfvswm67837pi1imy7hqfsdxjxdsnljb";
    rev = "60199f415e056d70f8f4426d6d3c6cd41e434c83";
  };
  postUnpack = "sourceRoot+=/cborg; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base bytestring containers ghc-prim half integer-gmp
    primitive text
  ];
  description = "Concise Binary Object Representation";
  license = stdenv.lib.licenses.bsd3;
}
