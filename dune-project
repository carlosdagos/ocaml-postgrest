(lang dune 2.0)

(generate_opam_files true)

(package
 (name postgrest)
 (synopsis "An OCaml library for talking to a PostgREST instance")
 (description "Describes methods useful to generate PostgREST queries")
 (depends
      core
      cohttp
      cohttp-lwt
      cohttp-lwt-unix
      lwt.unix
      yojson
      ppx_deriving_yojson))
