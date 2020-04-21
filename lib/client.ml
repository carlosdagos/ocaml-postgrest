open Types
open Lwt
open Cohttp
open Cohttp_lwt_unix
module B = Base
module J = Yojson.Safe

type auth =
  [ `NoAuth
  | `Userinfo of string * string
  | `Jwt of string
  ]

module type ClientInfo = sig
  val auth : auth

  val scheme : string

  val host : string

  val port : int
end

module Client (R : Record) (C : ClientInfo) = struct
  let make_uri ?(scheme = C.scheme) ?(host = C.host) ?(port = C.port) path qs =
    let uri_string =
      B.Printf.sprintf "%s://%s:%s/%s?%s" scheme host (string_of_int port) path
        qs
    in
    Uri.of_string uri_string

  let req ?(expected_code = 200) meth uri =
    Client.call meth uri >>= fun (resp, body') ->
    let code = resp |> Response.status |> Code.code_of_status in
    let body = body' |> Cohttp_lwt.Body.to_string in
    if code != expected_code then
      Lwt.fail_with (B.Printf.sprintf "Failed with code: %i" code)
    else
      body

  let decode_list json =
    match json with
    | `List l -> B.List.map l ~f:R.decode
    | _ ->
      let jstr = J.to_string json in
      let msg = B.Printf.sprintf "Tried to decode a list but got: %s" jstr in
      raise (Yojson.Json_error msg)

  let select ?(columns = [ "*" ]) ?(where = `Boolean true) schema =
    let cols =
      B.(Printf.sprintf "select=%s" (String.concat ~sep:"," columns))
    in
    let where_ = string_of_condition where in
    let qs = B.Printf.sprintf "%s&%s" cols where_ in
    let uri = make_uri schema qs in
    Lwt.map
      (B.Fn.compose decode_list J.from_string)
      (req `GET uri ~expected_code:200)
end

module DefaultClient = Client (JsonRecord)
module DefaultClientUnsafe = Client (JsonRecordUnsafe)
