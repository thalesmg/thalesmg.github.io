{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
      };
      hpkgs = pkgs.haskellPackages.override {
        overrides = hSelf: hSuper: {
          pandoc =
            pkgs.lib.pipe
              (hSelf.callHackage "pandoc" "2.11.4" {})
              [
                doJailbreak
                dontCheck
              ];
          tmg-site-builder = hSelf.callCabal2nix "site" ./site {};
          hakyll = pkgs.lib.pipe
            hSuper.hakyll
            [
              unmarkBroken
              doJailbreak
            ];
        };
      };
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      unmarkBroken = pkgs.haskell.lib.unmarkBroken;
      dontCheck = pkgs.haskell.lib.dontCheck;
      site-builder = hpkgs.tmg-site-builder;
      site-dir = ./site;
      site = pkgs.runCommand "tmg-site" {
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      } ''
        mkdir -p dist
        mkdir -p $out
        cp -R ${site-dir}/* dist/
        cd dist
        ${site-builder}/bin/site clean
        ${site-builder}/bin/site build
        cp -r _site/* $out/
      '';
    in
      {

        # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

        defaultPackage.x86_64-linux = site;
        devShell.x86_64-linux = hpkgs.shellFor {
          packages = p: [
            p.tmg-site-builder
          ];
          buildInputs = [
            pkgs.cabal-install
            hpkgs.hakyll
          ];
        };
      };
}
