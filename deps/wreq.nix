{ mkDerivation, aeson, aeson-pretty, attoparsec, authenticate-oauth
, base, base16-bytestring, base64-bytestring, byteable, bytestring
, case-insensitive, containers, cryptohash, directory, doctest
, exceptions, fetchgit, filepath, ghc-prim, hashable, http-client
, http-client-tls, http-types, HUnit, lens, lens-aeson, mime-types
, network-info, psqueues, QuickCheck, snap-core, snap-server
, stdenv, template-haskell, temporary, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, time-locale-compat, transformers, unix-compat
, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "wreq";
  version = "0.5.1.0";
  src = fetchgit {
    url = "https://github.com/bos/wreq";
    sha256 = "15rjl5jirbsn8ihg12ma4nkl7hab8sm4w0racnn2symh6hmbvx0i";
    rev = "7da9c07b21c8365a61b33ee379ee20aced40a50f";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec authenticate-oauth base base16-bytestring byteable
    bytestring case-insensitive containers cryptohash exceptions
    ghc-prim hashable http-client http-client-tls http-types lens
    lens-aeson mime-types psqueues template-haskell text time
    time-locale-compat unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-pretty base base64-bytestring bytestring
    case-insensitive containers directory doctest filepath hashable
    http-client http-types HUnit lens lens-aeson network-info
    QuickCheck snap-core snap-server temporary test-framework
    test-framework-hunit test-framework-quickcheck2 text time
    transformers unix-compat unordered-containers uuid vector
  ];
  homepage = "http://www.serpentine.com/wreq";
  description = "An easy-to-use HTTP client library";
  license = stdenv.lib.licenses.bsd3;
}
