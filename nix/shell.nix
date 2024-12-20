{
  pkgs ? import <nixpkgs> { },
  agda2hs-hs ? (import ./lib.nix { inherit pkgs; }).agda2hs-hs,
}:
agda2hs-hs.env.overrideAttrs (prev: {
  nativeBuildInputs = prev.nativeBuildInputs ++ (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    haskell-language-server
    pkgs.agda
  ]);
})
