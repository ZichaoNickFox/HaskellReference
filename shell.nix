{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc884.ghcWithPackages (p: [
    p.hspec
    p.megaparsec
    p.text
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}