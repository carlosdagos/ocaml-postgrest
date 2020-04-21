open Core
module B = Base
module J = Yojson.Safe

exception Could_Not_Decode

(* eeeh, is the builtin `Result` not the same as `Base.Result`? *)
let json_deriving_decoder_to_result ~f j =
  match f j with
  | Result.Ok t -> B.Result.Ok t
  | Result.Error s -> B.Result.Error s

let json_deriving_decoder_to_unsafe ~f j =
  match f j with
  | Result.Ok t -> t
  | Result.Error _ -> raise Could_Not_Decode
