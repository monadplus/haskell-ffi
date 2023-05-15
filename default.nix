{ compiler ? "ghc927" }:

let
  sources = import ./nix/sources.nix;

  inherit (import sources."gitignore.nix" { inherit (pkgs) lib; }) gitignoreSource;

  nixpkgs = sources.nixpkgs;

  config = { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override (old: {
          overrides =
            let
              packageSources =
                self.haskell.lib.packageSourceOverrides {
                  "haskell-ffi" = gitignoreSource ./.;
                };

              # Not actually needed
              manualOverrides = haskPkgsNew: haskPkgsOld:
                {
                  haskell-ffi =
                    haskPkgsOld.haskell-ffi.override {
                      pcre = super.pcre;
                    };
                };

              default = old.overrides or (_: _: { });

            in
            self.lib.fold self.lib.composeExtensions default [
              packageSources
              manualOverrides
            ];
        });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit config;
    overlays = [ overlay ];
  };

in
{
  inherit (pkgs.haskell.packages."${compiler}") haskell-ffi;
  shell =
    (pkgs.haskell.packages."${compiler}".haskell-ffi).env.overrideAttrs (
      old: with pkgs.haskell.packages."${compiler}"; {
        nativeBuildInputs = old.nativeBuildInputs ++ [
          ghcid
          haskell-language-server
          ormolu
        ];
        shellHook = ''
          source .config/secrets
        '';
      }
    );
}
