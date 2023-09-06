{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.pact-lsp.flake {
        # crossPlatforms = p: [ p.ghcjs ];
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact-lsp =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pkgconfig
                python3 python3Packages.sphinx python3Packages.sphinx_rtd_theme
                pandoc perl
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
        })
      ];
    in flake // rec {
      packages.default = flake.packages."pact-lsp:exe:pact-lsp";


      devShell = pkgs.haskellPackages.shellFor {
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
        ];

        withHoogle = true;
      };
    });
}
