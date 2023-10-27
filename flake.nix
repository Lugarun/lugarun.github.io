{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    panpipe = {
      url = "github:Lugarun/panpipe";
      flake = false;
    };
    panhandle = {
      url = "github:Lugarun/panhandle";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, panpipe, panhandle}:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          packages.site-builder = pkgs.haskellPackages.developPackage {
            root = ./.;
            source-overrides = {
              panpipe = panpipe;
              panhandle = panhandle;
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [ cabal-install
                ghcid
                pkgs.zlib
              ]);
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.marksman
              pkgs.rubber
              pkgs.texlive.combined.scheme-full
              pkgs.poppler_utils
              pkgs.gnuplot
              (pkgs.python3.withPackages (ps: with ps; [
                pandas
                matplotlib
              ]))
            ];
          };
        }
    );
}
