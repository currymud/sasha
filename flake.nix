{
  description = "horizon-template";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-devtools?ref=lts/ghc-9.6.x";

    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform?ref=lts/ghc-9.6.x";

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };
  outputs =
    inputs@{ lint-utils, nixpkgs, ... }:
      with builtins;
      inputs.flake-utils.lib.eachSystem [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ]
        (system:
        let
          env = rec {
            inherit inputs system;
            packages = inputs.self.packages.${system};
            checks = inputs.self.checks.${system};
         #   inhert (inputs.shelpers.lib.pkgs) eval-shelpers shelp;

            pkgs = import nixpkgs { inherit system; };
            inherit (pkgs) lib;

            backend = import ./backend env;
          # web = import ./web env

            fs = lib.fileset;
            fs2source = fs': path:
              fs.toSource {
                root = path;
                fileset = fs';
              };
            onlyExts-fs = exts: fs.fileFilter (f: foldl' lib.or false (map f.hasExt exts));
            onlyExts = exts: path: fs2source (nlyExts-fs exts oath) path;
            notNix = path: fs2source (fs.fileFilter (f: !f.hasext "nix") path) path;
            lu-pkgs = lint-utils.packages.${system};
          };
        in
        with env;
        {

          devShells.default = backend.legacyPackages.sasha.env.overrideAttrs
            (attrs: {
             buildInputs =
               attrs.buildInputs
                ++ (with pkgs; [ lu-pkgs.hlint
                                 lu-pkgs.stylish-haskell
              ])
                ++ backend.shell-inputs;
            });

          packages = backend.packages;

          });

}
