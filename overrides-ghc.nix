{ reflex-platform, ... }:
let
  c2n = reflex-platform.cabal2nixResult;
  dc  = reflex-platform.lib.dontCheck;
in reflex-platform.ghc.override {
  overrides = self: super: {
    # servant-snap        = dc (self.callPackage (c2n deps/servant-snap) {});
    # heist               = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/heist) {});
    # xmlhtml             = dc (self.callPackage (c2n deps/servant-snap/deps/snap/deps/xmlhtml) {});
    snap                  = dc (self.callPackage deps/snap.nix {});
    snaplet-acid-state    = dc (self.callPackage deps/snaplet-acid-state.nix {});
    cborg                 = dc (self.callPackage deps/cborg.nix {});
    serialise             = dc (self.callPackage deps/serialise.nix {});
    wreq                  = dc (self.callPackage deps/wreq.nix {});
  };
}
