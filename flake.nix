{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) lib;
      in {
        formatter = pkgs.nixfmt;

        packages.eglot-semantic-tokens = with pkgs.emacs.pkgs;
          trivialBuild {
            pname = "eglot-semantic-tokens";
            version = "master";

            src = ./eglot-semantic-tokens.el;

            meta = with lib; {
              description = "eglot semantic tokens support";
              homepage =
                "https://codeberg.org/eownerdead/eglot-semantic-tokens";
              license = licenses.gpl3Only;
              platforms = platforms.all;
            };
          };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ editorconfig-checker nixfmt ];
        };
      });
}
