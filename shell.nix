{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc963.ghcWithPackages (p: [
    p.hspec
    p.megaparsec
    p.text
    p.typerep-map
    p.http-types
    p.wai
    p.vault
    p.split
    p.lens
    p.wavefrontOBJ
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}