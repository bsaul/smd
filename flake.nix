{
  description = "smd R package";
  nixConfig = {
    bash-prompt = "smd> ";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachDefaultSystem (system: let
      
      package = "smd";
      version = "0.7.0";

      pkgs = nixpkgs.legacyPackages.${system};

      inherit (gitignore.lib) gitignoreSource;

      smdImports = with pkgs.rPackages; [
            MASS
          ];

      smdSuggests = with pkgs.rPackages; [ 
          testthat 
          stddiff
          tableone
          knitr
          dplyr
          purrr
          markdown
          rmarkdown
        ];

    in {

      packages.${package} = pkgs.rPackages.buildRPackage {
          name = "smd";
          src = gitignoreSource ./.;
          propagatedBuildInputs = smdImports;
        };
      
      # Run with
      # nix build .#cran
      packages.cran = pkgs.stdenv.mkDerivation {
          name = "cran";
          version = version;
          src = gitignoreSource ./.;
          buildInputs = [ 
              pkgs.R 
              pkgs.pandoc
              pkgs.rPackages.devtools
              pkgs.qpdf
              pkgs.texlive.combined.scheme-full
            ] ++ smdSuggests ++ smdImports ;
          doCheck = true;
          buildPhase = ''
            ${pkgs.R}/bin/Rscript -e 'devtools::document()'
            ${pkgs.R}/bin/R CMD build .
          '';
          # NOTE: 
          # Not all checks will pass because R CMD check does stuff
          # This one gives warning:
          # * checking R files for syntax errors ... WARNING
          #  OS reports request to set locale to "en_US.UTF-8" cannot be honored
          # I don't know how to set the locale in a derivation 
          # (or if that's even possible)
          # Others are notes (e.g.):
          # * Found the following (possibly) invalid URLs:
          # R CMD check wants to access the interwebs but nix no like that.
          checkPhase = ''
            ${pkgs.R}/bin/R CMD check $(ls -t . | head -n1) --as-cran
          '';
          installPhase  = ''
            mkdir -p $out
            cp ${package}_${version}.tar.gz $out
            cp -r ${package}.Rcheck/ $out/logs
          '';
      };
      
      packages.default = self.packages.${system}.${package};

      devShells.default =  pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [
          pkgs.R
          pkgs.rPackages.devtools
          pkgs.rPackages.usethis
          pkgs.rPackages.styler
          pkgs.rPackages.lintr
        ] ++ smdImports ++ smdSuggests ;
      }; 

    });
}
