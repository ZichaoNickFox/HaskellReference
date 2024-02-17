{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc963.ghcWithPackages (p: [
    p.aeson
    p.aeson-qq
    p.data-default
    p.data-has
    p.hspec
    p.http-types
    p.lens
    p.lens-aeson
    p.megaparsec
    p.pcre-heavy
    p.safe
    p.split
    p.stm
    p.text
    p.typerep-map
    p.vault
    p.wai
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}