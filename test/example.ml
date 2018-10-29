open Postgrest.Client

module U = Postgrest.Util

type todo =
  { done' : bool [@key "done"];
    due : string option;
    id : int;
    task : string
  } [@@deriving yojson]

module C =
  Client(struct
      type t = todo
      type return_t = t
      let decode = U.json_deriving_decoder_to_unsafe ~f:todo_of_yojson
      let encode = todo_to_yojson
    end)(struct
      let auth = `NoAuth
      let scheme = "http"
      let host = "localhost"
      let port = 3000
    end)

let%expect_test _ =
  print_endline "It works!";
  [%expect{|
    It works!
  |}]
