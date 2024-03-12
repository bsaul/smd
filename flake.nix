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
      
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (gitignore.lib) gitignoreSource;

      smdDeps = with pkgs.rPackages; [
            MASS
          ];
  
    in {

      packages.smd = pkgs.rPackages.buildRPackage {
          name = "smd";
          src = gitignoreSource ./.;
          propagatedBuildInputs = smdDeps;
        };
      
      packages.default = self.packages.${system}.smd;

      devShells.default =  pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [
          pkgs.R
          pkgs.rPackages.devtools
          pkgs.rPackages.usethis
        ] ++ smdDeps ;
      }; 

    });
}
