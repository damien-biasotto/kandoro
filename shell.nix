{pkgs ?  import (fetchTarball https://github.com/nixos/nixpkgs/archive/nixpkgs-unstable.tar.gz) {} }:

let 
  shellBuildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test-rs
    elmPackages.elm-review
    elmPackages.elm-analyse
    nodejs
    elm2nix
    watch
  ];

in
pkgs.mkShell {
  buildInputs = shellBuildInputs;

  shellHook = ''
  '';
}
