{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.hakyll-flakes.url = "github:Radvendii/hakyll-flakes";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, hakyll-flakes, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system: nixpkgs.lib.recursiveUpdate (
        hakyll-flakes.lib.mkAllOutputs {
          inherit system;
          name = "lugarun";
          src = ./.;
          websiteBuildInputs = with nixpkgs.legacyPackages.${system}; [
            rubber
            texlive.combined.scheme-full
            poppler_utils
            gnuplot
          ];
        }) {
          packages = { inherit (nixpkgs.legacyPackages.${system}) gnuplot rubber poppler_utils; texlive = nixpkgs.legacyPackages.${system}.texlive.combined.scheme-full; };
          devShells.default = nixpkgs.legacyPackages.${system}.mkShell {
            buildInputs = with nixpkgs.legacyPackages.${system}; [
              rubber
              texlive.combined.scheme-full
              poppler_utils
              gnuplot
              marksman
              (python3.withPackages (ps: with ps; [
                pandas
                matplotlib
                ]))
            ];
          };
        }
    );
}
