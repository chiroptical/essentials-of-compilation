{
  pkgs,
  snail-shell,
  ...
}:
pkgs.mkShell {
  inputsFrom = [
    (import ./essentialsOfCompilation.nix {
      inherit pkgs;
      inherit snail-shell;
    })
    .env
  ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.retrie
    haskellPackages.fourmolu
    haskellPackages.haskell-language-server
    alejandra
  ];
  withHoogle = true;
  LANG = "en_US.utf8";
}
