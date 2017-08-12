{ mkDerivation, acid-state, base, fetchgit, mtl, snap, stdenv, text
, transformers
}:
mkDerivation {
  pname = "snaplet-acid-state";
  version = "0.2.7";
  src = fetchgit {
    url = "https://github.com/imalsogreg/snaplet-acid-state";
    sha256 = "0bwzp1gf8rncpfa6vyvzl22371kcjvkzswmvyvcm8n0jj5dhbmm2";
    rev = "ccf04965f6a606ff06e209a6a16af1b7855bd270";
  };
  libraryHaskellDepends = [
    acid-state base mtl snap text transformers
  ];
  homepage = "https://github.com/mightybyte/snaplet-acid-state";
  description = "acid-state snaplet for Snap Framework";
  license = stdenv.lib.licenses.bsd3;
}
