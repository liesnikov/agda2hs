{
  pkgs ? import <nixpkgs> { },
  agda2hs-hs ? (import ./lib.nix { inherit pkgs; }).agda2hs-hs,
}:
let
  hpkgs = pkgs.haskellPackages.extend (final: prev: {agda2hs = agda2hs-hs});
in
hpkgs.shellFor {
  packages = p: [ p.agda2hs ];
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    haskell-language-server
    pkgs.agda
  ];
}
