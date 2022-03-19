{
  inputs.hakyll = {
    url = "github:jaspervdj/hakyll/master";
    flake = false;
  };

  outputs = { self, nixpkgs, hakyll}:
    let
      inherit (builtins) filterSource;
      inherit (nixpkgs.lib) flip;
      inherit (pkgs.nix-gitignore) gitignoreSourcePure;

      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
      src = gitignoreSourcePure [ ./.gitignore ] ./.;

    in {
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
              hakyll = hself.callCabal2nix "hakyll" hakyll { };
              builder = prev.haskell.lib.justStaticExecutables
                (hself.callCabal2nix "builder" src { });
            });
        };

        blog-builder = final.haskellPackages.builder;
        blog = final.stdenv.mkDerivation {
          name = "blog";
          src = src;
          phases = "unpackPhase buildPhase";
          buildInputs = [ final.blog-builder ];
          buildPhase = ''
            export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
            export LANG=en_US.UTF-8
            site build
            mkdir -p $out
            cp -r _site/* $out
          '';
        };
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.blog;
      packages.x86_64-linux = {
        inherit (pkgs) blog-builder blog;
      };
      devShell.x86_64-linux =
        pkgs.mkShell {
          nativeBuildInputs = [ pkgs.blog-builder pkgs.pandoc ];
        };
    };
}
