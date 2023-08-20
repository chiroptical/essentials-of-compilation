{
  pkgs,
  snail-shell,
  ...
}: let
  haskell = pkgs.callPackage ./haskell.nix {
    inherit pkgs;
    inherit snail-shell;
  };
in
  haskell.callCabal2nix "essentialsOfCompilation" ./. {}
