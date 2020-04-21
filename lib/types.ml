module B = Base
module J = Yojson.Safe

module type Record = sig
  type t

  type return_t

  val decode : J.t -> return_t

  val encode : t -> J.t
end

module JsonRecord = struct
  type t = J.t

  type return_t = (t, string) B.Result.t

  let decode j = B.Result.Ok j

  let encode = B.Fn.id
end

module JsonRecordUnsafe = struct
  type t = J.t

  type return_t = t

  let decode = B.Fn.id

  let encode = B.Fn.id
end

(* As described in
 * http://postgrest.org/en/v5.1/api.html#horizontal-filtering-rows
 *)
type operator =
  [ `Eq (* Equals *)
  | `Gt (* Greater Than *)
  | `Gte (* Greater Than or Equal *)
  | `Lt (* Less Than *)
  | `Lte (* Less Than or Equal *)
  | `Neq (* Not Equal *)
  | `Like (* LIKE Operator *)
  | `ILike (* ILIKE Operator *)
  | `In (* One in a list of values *)
  | `Is (* Checking for exact equality *)
  | `Fts of string option (* Full text search using to_tsquery *)
  | `Plfts of string option (* Full text search using plainto_tsquery *)
  | `Phfts of string option (* full text search using phraseto_tsquery *)
  | `Cs (* Contains *)
  | `Cd (* Contained in *)
  | `Ov (* Overlap (like for dates) *)
  | `Sl (* Strictly left of *)
  | `Sr (* Strictly right of *)
  | `Nxr (* Does not extend to the right *)
  | `Nxl (* Does not extend to the left *)
  | `Adj (* Is adjacent to *)
  ]

type logical_operator =
  [ `And
  | `Or
  ]

type simple_value_t =
  [ `Null
  | `Bool of bool
  ]

type string_value_t = [ `String of string ]

type numeric_value_t =
  [ `Int of int
  | `Float of float
  ]

type value_t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Stringlit of string
  ]

type condition =
  [ `Boolean of bool
  | `Operator of operator_t
  | `Combined of combined_t
  ]

and operator_t =
  { op : operator
  ; negated_op : bool
  ; attribute : string
  ; value : value_t
  }

and combined_t =
  { negated_comb : bool
  ; op_combined : logical_operator
  ; conditions : condition list
  }

let combine_with logical_operator a b =
  (* Combine any two conditions *)
  let combine_simple a' b' =
    `Combined
      { negated_comb = false
      ; op_combined = logical_operator
      ; conditions = [ a'; b' ]
      }
  in
  (* Add any condition `b'` to all the conditions of `a'` *)
  let combine_into a' b' =
    `Combined { a' with conditions = B.List.append a'.conditions [ b' ] }
  in
  (* Combine two `combined_t` *)
  let combine_tuple a' b' =
    if a'.negated_comb = b'.negated_comb && a'.op_combined = b'.op_combined then
      `Combined
        { negated_comb = a'.negated_comb
        ; op_combined = a'.op_combined
        ; conditions = List.append a'.conditions b'.conditions
        }
    else
      combine_into a' (`Combined b')
  in
  match (a, b) with
  | `Combined a', `Combined b' -> combine_tuple a' b'
  | `Combined a', _ ->
    if a'.op_combined = logical_operator then
      combine_into a' b
    else
      combine_simple a b
  | _, `Combined b' ->
    if b'.op_combined = logical_operator then
      combine_into b' a
    else
      combine_simple b a
  | _, _ -> combine_simple a b

(* Combined conjunction *)
let and' a b = combine_with `And a b

(* Alias for `and'` *)
let ( &&. ) = and'

(* Combined conjunction *)
let and'' l =
  let combined = { negated_comb = false; op_combined = `And; conditions = l } in
  `Combined combined

(* Combined disjunction *)
let or' a b = combine_with `Or a b

(* Alias for `or'` *)
let ( ||. ) = or'

(* Combined disjunction *)
let or'' l =
  let combined = { negated_comb = false; op_combined = `Or; conditions = l } in
  `Combined combined

(* Negate a condition *)
let not' cond =
  match cond with
  | `Boolean b -> `Boolean (not b)
  | `Operator o ->
    let updated = { o with negated_op = not o.negated_op } in
    `Operator updated
  | `Combined c ->
    let updated = { c with negated_comb = not c.negated_comb } in
    `Combined updated

(* String functions *)

let string_of_operator (o : operator) =
  let fts_map lang fts_name =
    B.(
      Option.value_map lang ~default:fts_name
        ~f:(Printf.sprintf "%s(%s)" fts_name))
  in
  match o with
  | `Eq -> "eq"
  | `Gt -> "gt"
  | `Gte -> "gte"
  | `Lt -> "lt"
  | `Lte -> "lte"
  | `Neq -> "neq"
  | `Like -> "like"
  | `ILike -> "ilike"
  | `In -> "in"
  | `Is -> "is"
  | `Cs -> "cs"
  | `Cd -> "cd"
  | `Ov -> "ov"
  | `Sl -> "sl"
  | `Sr -> "sr"
  | `Nxr -> "nxr"
  | `Nxl -> "nxl"
  | `Adj -> "adj"
  | `Fts lang -> fts_map lang "fts"
  | `Plfts lang -> fts_map lang "plfts"
  | `Phfts lang -> fts_map lang "phfts"

let string_of_logical_operator (lo : logical_operator) =
  match lo with
  | `And -> "and"
  | `Or -> "or"

let string_of_value (v : value_t) =
  match v with
  | `Null -> "null"
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> Uri.pct_encode ~component:`Query_value s
  | `Stringlit s -> s

let string_of_value_list (v : value_t list) =
  let concmap =
    B.(Fn.compose (String.concat ~sep:",") (List.map ~f:string_of_value))
  in
  B.Printf.sprintf "(%s)" (concmap v)

let rec string_of_condition ?(tl = true) c =
  match c with
  | `Boolean b -> string_of_bool b
  | `Operator o ->
    let negated_str =
      if o.negated_op then
        "not."
      else
        ""
    in
    let operator_str = string_of_operator o.op in
    let fmt =
      B.Printf.(
        if tl then
          sprintf "%s=%s%s.%s"
        else
          sprintf "%s.%s%s.%s")
    in
    fmt o.attribute negated_str operator_str (string_of_value o.value)
  | `Combined c ->
    let negated_str =
      if c.negated_comb then
        "not."
      else
        ""
    in
    let operator_str = string_of_logical_operator c.op_combined in
    let fmt =
      B.Printf.(
        if tl then
          sprintf "%s%s=(%s)"
        else
          sprintf "%s%s(%s)")
    in
    fmt negated_str operator_str
      (B.String.concat ~sep:","
         (B.List.map c.conditions ~f:(string_of_condition ~tl:false)))

(* Condition constructors *)

let make_cond op attribute value : condition =
  let op_cond = { negated_op = false; op; attribute; value } in
  `Operator op_cond

let eq' = make_cond `Eq

let neq' a = B.Fn.(compose not' (eq' a))

let gt' a (v : numeric_value_t) = make_cond `Gt a (v :> value_t)

let gte' a (v : numeric_value_t) = make_cond `Gte a (v :> value_t)

let lt' a (v : numeric_value_t) = make_cond `Lt a (v :> value_t)

let lte' a (v : numeric_value_t) = make_cond `Lte a (v :> value_t)

let is' a (v : simple_value_t) = make_cond `Is a (v :> value_t)

let like' a (v : string_value_t) = make_cond `Like a (v :> value_t)

let ilike' a (v : string_value_t) = make_cond `ILike a (v :> value_t)

let fts' ?lang a (v : string_value_t) = make_cond (`Fts lang) a (v :> value_t)

let plfts ?lang a (v : string_value_t) = make_cond (`Plfts lang) a (v :> value_t)

let phfts ?lang a (v : string_value_t) = make_cond (`Phfts lang) a (v :> value_t)

let string_in' a (v : string_value_t list) =
  `Stringlit (string_of_value_list (v :> value_t list)) |> make_cond `In a

let number_in' a (v : numeric_value_t list) =
  `Stringlit (string_of_value_list (v :> value_t list)) |> make_cond `In a
