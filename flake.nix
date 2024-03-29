{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
      };
      hpkgs = pkgs.haskellPackages.override {
        overrides = hSelf: hSuper: {
          # pandoc =
          #   pkgs.lib.pipe
          #     (hSelf.callHackage "pandoc" "2.11.4" {})
          #     [
          #       doJailbreak
          #       dontCheck
          #     ];
          tmg-site-builder = hSelf.callCabal2nix "site" ./site {};
          # hakyll = pkgs.lib.pipe
          #   hSuper.hakyll
          #   [
          #     unmarkBroken
          #     doJailbreak
          #   ];
          # hls-fourmolu-plugin = dontCheck hSuper.hls-fourmolu-plugin;
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
        buildInputs = [
          pkgs.coqPackages.coq
          pkgs.coqPackages.serapi
          pkgs.python3Packages.alectryon
        ];
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
        defaultPackage.x86_64-linux = site;
        devShell.x86_64-linux = hpkgs.shellFor {
          packages = p: [
            p.tmg-site-builder
          ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            hpkgs.hakyll
            hpkgs.hakyll-alectryon
            pkgs.coqPackages.coq
            pkgs.coqPackages.serapi
            pkgs.python3Packages.alectryon
          ];
        };
      };
}
