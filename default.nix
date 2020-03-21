let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          regex =
            haskellPackagesNew.callPackage ./regex.nix {
              pcre = pkgs.pcre;
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    regex = pkgs.haskellPackages.regex;
  }
