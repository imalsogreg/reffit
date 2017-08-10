{ mkDerivation, acid-state, base, fetchgit, mtl, snap, stdenv, text
, transformers
}:
mkDerivation {
  pname = "snaplet-acid-state";
  version = "0.2.7";
  src = fetchgit {
    url = "https://github.com/imalsogreg/snaplet-acid-state";
    sha256 = "10v1dx8sdwzs7aynii564qk0pg7jd4pzk00s4abyjzk7mga9f3m0";
    rev = "a50bc7fc51dacd2a7adecdcca268ccf748879ed4";
  };
  libraryHaskellDepends = [
    acid-state base mtl snap text transformers
  ];
  homepage = "https://github.com/mightybyte/snaplet-acid-state";
  description = "acid-state snaplet for Snap Framework";
  license = stdenv.lib.licenses.bsd3;
}
