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
      version = "0.6.7";

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
      
      # Find a better way to build this derivation
      # sudo nix build --option sandbox 'relaxed' .#cran 
      packages.cran = pkgs.stdenv.mkDerivation {
          __noChroot = true; # WANT REMOVE THIS but 
                             # R CMD check requires internet access
                             # What's a better way?
          name = "cran";
          version = version;
          src = gitignoreSource ./.;
          buildInputs = [ 
              pkgs.R 
              pkgs.pandoc 
              pkgs.texlive.combined.scheme-full
            ] ++ smdSuggests ++ smdImports ;
          buildPhase = ''
            ${pkgs.R}/bin/R CMD build .  && \
              ${pkgs.R}/bin/R CMD check $(ls -t . | head -n1) --as-cran
          '';
          installPhase  = ''
            mkdir -p $out
            cp ${package}_${version}.tar.gz $out
          '';
      };
      
      packages.default = self.packages.${system}.${package};

      devShells.default =  pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [
          pkgs.R
          pkgs.rPackages.devtools
          pkgs.rPackages.usethis
        ] ++ smdImports ++ smdSuggests ;
      }; 

    });
}
