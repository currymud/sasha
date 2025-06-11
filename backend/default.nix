{ fs
, fs2source
, inputs
, lib
, onlyExts
, onlyExts-fs
, packages
, pkgs
, system
, ...
}:
let
  hlib = pkgs.haskell.lib;

  onlyHaskell = fs2source
    (fs.union
      (onlyExts-fs
        [
          "cabal"
          "hs"
          "project"
        ]
        ./.)
      ./LICENSE # horizon requires this file to build
    );

  myOverlay = final: _prev: {
    sasha = final.callCabal2nix "sasha" (onlyHaskell ./.) { };
    Earley = final.callHackage "Earley" "0.13.0.1" { };
    directory-tree = final.callHackage "directory-tree" "0.12.1" { };
  };

  legacyPackages =
    inputs.horizon-platform.legacyPackages.${system}.extend
      myOverlay;

  devtools = inputs.horizon-devtools.packages.${system};

  oneTarget = target:
    let
      package =
        # Do not use profiling or add documentation. These are not only
        # wasteful, but cause the build to fail, by triggering a second compilation.
        hlib.justStaticExecutables
          # Don't configure the project to run tests, we are building only one binary here.
          # Tests will fail as well as some suites are integration tests with environmental
          # requirements we provide in the Nix VM Tests.
          (hlib.dontCheck
            # Set the build target to the exe alone
            (hlib.setBuildTarget legacyPackages.sasha target));
    in
    if lib.hasPrefix "exe:" target then
      package.overrideAttrs
        { meta.mainProgram = lib.removePrefix "exe:" target; }
    else
      package;

in
{
  inherit legacyPackages;

  shell-inputs = with pkgs; [
    cabal-install
    zlib
  ]
  ++ lib.optionals (system == "x86_64-linux")
    (with pkgs; [
      devtools.ghcid
      devtools.haskell-language-server
    ]);

  packages = {
    server = oneTarget "exe:redshift-server";
    migration = oneTarget "exe:migration";
  };

}
