(import ./deps/reflex-platform {}).project ({ pkgs, ... }:
let cabal2nix = src: pkgs.runCommand "cabal2nix" {
        buildCommand = ''
            cabal2nix file://"${builtins.filterSource (path: type: path != ".git") src}" > $out
        '';
        buildInputs = [
            pkgs.cabal2nix
        ];
    } "";
in {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["frontend" "common"];
  };

  overrides = self: super: {
    heist = pkgs.haskell.lib.doJailbreak super.heist;

    # TODO: use fetchFromGithub and use runCabal2Nix instead of defining cabal2nix as above.

    #snaplet-persistent = pkgs.haskellPackages.callCabal2nix "snaplet-persistent" (pkgs.fetchgit {
    #    url = "git://github.com/fschlaepfer/snaplet-persistent.git";
    #    rev = "b4225da789c666606cdfa87cd5cffaff48ffaed7";
    #    sha256 = "1w5ms0k2d2mglp6b2kwgy4w73jnkwrd3ych22qzy5n947np4js0g";
    #}) {};
    snaplet-persistent = self.callPackage (cabal2nix ./deps/snaplet-persistent) {};
    #postgresql-simple = if self.ghc.isGhcjs or false then null else super.postgresql-simple;
  };
})
