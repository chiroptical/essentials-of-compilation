{pkgs, snail-shell, ...}:
# let aeson = pkgs.callPackage ./nix/aeson.nix {};
# in
pkgs.haskell.packages.ghc94.extend (final: prev: {
  "snail-shell" = snail-shell;
  # "aeson" =
  #   final.callCabal2nix
  #   "aeson"
  #   aeson
  #   {};
})
