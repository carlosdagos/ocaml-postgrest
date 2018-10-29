{ nixpkgs ? (import <nixpkgs> {}) }:

let
  ocaml-postgrest = { stdenv, ocamlPackages, dune }:
    stdenv.mkDerivation rec {
      version = "0.0.0";
      name = "ocaml-postgrest-${version}";

      src = ./.;

      propagatedBuildInputs = with ocamlPackages; [
          core cohttp cohttp-lwt cohttp-lwt-unix
          yojson ppx_deriving_yojson
      ];

      buildInputs = with ocamlPackages; [
          ocaml
          findlib
          dune
      ];

      inherit (dune) installPhase;
    };
in
nixpkgs.ocamlPackages.callPackage ocaml-postgrest {}
