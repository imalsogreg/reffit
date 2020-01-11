let

  nixpkgs_bootstrap = import <nixpkgs> {};

  # nixpkgs = nixpkgs_bootstrap.fetchFromGitHub {
  #        owner = "nixos";
  #        repo = "nixpkgs-channels";
  #        rev = "67135fbcc5d5d28390c127ef519b09a362ef2466";
  #        sha256  = "1xsaz9n41p8yxqxf78lh74bbpvgnymdmq1hvnagra7r6bp3jp7ad";
  #  };
   nixpkgs = nixpkgs_bootstrap.fetchFromGitHub {
         owner = "nixos";
         repo = "nixpkgs-channels";
         rev = "78e9665b48ff45d3e29f45b3ebeb6fc6c6e19922";
         sha256  = "09f50jaijvry9lrnx891qmcf92yb8qs64n1cvy0db2yjrmxsxyw8";
   };

   reffit-overlay = self: super:
     let
       dc = self.haskell.lib.dontCheck;
       dh = self.haskell.lib.dontHaddock;
       dj = self.haskell.lib.doJailbreak;
       pcfg = self.haskell.lib.addPkgconfigDepends;
     in
       {
         icuuc = self.icu;
         "icu-uc" = self.icu;
         haskellPackages = super.haskellPackages.override {
           overrides = helf: huper: {
             map-syntax         = dj huper.map-syntax;
             heist              = dc   huper.heist;
             http-streams       = dc   huper.http-streams;
             digestive-functors-heist = dh huper.digestive-functors-heist;
             snap                     = dc huper.snap;
             snaplet-acid-state = dj huper.snaplet-acid-state;
             reffit             =
               (pcfg (helf.callCabal2nix "reffit" ../. {}) [self.icu59] ).overrideDerivation( oldAttrs:  {
                 propagatedBuildInputs = [self.pandoc];
               });
           };
         };
       };
in
import <nixpkgs> { overlays = [reffit-overlay]; }
