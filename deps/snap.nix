{ mkDerivation, aeson, async, attoparsec, base, bytestring, cereal
, clientsession, configurator, containers, deepseq, directory
, directory-tree, dlist, fetchgit, filepath, Glob, hashable, heist
, http-streams, HUnit, lens, lifted-base, map-syntax, monad-control
, mtl, mwc-random, pwstore-fast, QuickCheck, smallcheck, snap-core
, snap-server, stdenv, stm, syb, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-smallcheck, text, time, transformers
, transformers-base, unordered-containers, xmlhtml
}:
mkDerivation {
  pname = "snap";
  version = "1.1.0.0";
  src = fetchgit {
    url = "https://github.com/imalsogreg/snap";
    sha256 = "1dp8rqgy75wzh7h12h97rp0880q65jyk72vnbpcfwbvnpmfidbsy";
    rev = "33cfa019024abe41d2d34a1178069bf27bc8c2c2";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring cereal clientsession configurator
    containers directory directory-tree dlist filepath hashable heist
    lens lifted-base map-syntax monad-control mtl mwc-random
    pwstore-fast snap-core snap-server stm text time transformers
    transformers-base unordered-containers xmlhtml
  ];
  testHaskellDepends = [
    aeson async attoparsec base bytestring cereal clientsession
    configurator containers deepseq directory directory-tree dlist
    filepath Glob hashable heist http-streams HUnit lens lifted-base
    map-syntax monad-control mtl mwc-random pwstore-fast QuickCheck
    smallcheck snap-core snap-server stm syb test-framework
    test-framework-hunit test-framework-quickcheck2
    test-framework-smallcheck text time transformers transformers-base
    unordered-containers xmlhtml
  ];
  homepage = "http://snapframework.com/";
  description = "Top-level package for the Snap Web Framework";
  license = stdenv.lib.licenses.bsd3;
}
