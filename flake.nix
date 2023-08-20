{
  description = "Working on Jeremy Siek's Book";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    snail-shell-src.url = "github:chiroptical/snail-shell";
  };

  outputs = {
    # self,
    nixpkgs,
    flake-utils,
    snail-shell-src,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      snail-shell = snail-shell-src.packages.${system}.snail-shell;
      essentialsOfCompilation = pkgs.callPackage ./essentialsOfCompilation.nix {
        inherit pkgs;
        inherit snail-shell;
      };
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit snail-shell;
      };
      defaultPackage = essentialsOfCompilation;
      packages = flake-utils.lib.flattenTree {
        inherit essentialsOfCompilation;
      };
    });
}
