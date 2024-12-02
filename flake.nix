{
  description = "Advent of Code 2024 solutions in Haskell.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable?shallow=1";
  };

  outputs = {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config = {allowUnfree = true;};
    };
    inherit (nixpkgs) lib;

    days =
      lib.pipe (builtins.readDir ./.)
      [
        (lib.attrsets.filterAttrs (key: type: lib.strings.hasPrefix "day" key && type == "directory"))
        builtins.attrNames
        (
          map (
            d: {
              name = d;
              value = pkgs.haskellPackages.developPackage {
                root = ./. + "/${d}";
              };
            }
          )
        )
        builtins.listToAttrs
      ];
  in {
    formatter.${system} = pkgs.alejandra;

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        aoc-cli
        cabal-install
        ghc
        haskell-language-server
      ];
    };

    packages.${system} = days;
  };
}
