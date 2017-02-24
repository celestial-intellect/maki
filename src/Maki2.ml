
(** {1 Maki} *)

module E = CCResult
module Fmt = CCFormat

type 'a or_error = ('a, exn) CCResult.t

type -'a op = {
  pp: 'a CCFormat.printer;
  hash: 'a -> Sha1.t;
}

let make_op pp hash: _ op = {pp; hash}

let hash_str s = Sha1.string s
let hash_map f x = hash_str (f x)
let hash_pp pp x = hash_str (Fmt.to_string pp x)

let unit = make_op Fmt.unit (fun () -> Sha1.zero)
let int = make_op Fmt.int (hash_map string_of_int)
let bool = make_op Fmt.bool (hash_map string_of_bool)
let option o = let pp = Fmt.Dump.option o.pp in make_op pp (hash_pp pp)
let list o = let pp = Fmt.Dump.list o.pp in make_op pp (hash_pp pp)
let list_set o =
  let pp = Fmt.Dump.list o.pp
  and hash l =
    (* hash elements, then sort hashes to ensure commutativity *)
    let l = List.map o.hash l |> List.map Sha1.to_bin |> List.sort Pervasives.compare in
    Sha1.string (String.concat ";" l)
  in make_op pp hash

type 'a codec = {
  encode: 'a -> string;
  decode: string -> 'a or_error;
}

type _ t =
  | Pure : 'a op * 'a -> 'a t
  | Catch : 'a or_error t -> 'a t
  | Lwt : 'a Lwt.t t -> 'a t
  | If : bool t * 'a t * 'a t -> 'a t
  | App : ('f, 'ret) args * 'f -> 'ret t
  | Seq : unit t * 'a t -> 'a t
  | On_start : (unit -> unit) * 'a t -> 'a t
  | On_done : ('a or_error -> unit) * 'a t -> 'a t
  | L_map : int t * ('a -> 'b) t * 'a list t -> 'b list t
  | L_flat_map : int t * ('a -> 'b list) t * 'a list t -> 'b list t

and ('a, 'ret) args =
  | Arg_compute : 'ret op -> ('ret, 'ret) args
  | Arg_memo : string * 'ret codec -> (unit -> 'ret, 'ret) args
  | Arg_app : 'a t * ('b, 'ret) args -> ('a -> 'b, 'ret) args

let pure op x = Pure (op,x)

let seq a b = Seq (a,b)

let (>>) = seq

let if_ a b c = If (a,b,c)

let catch x = Catch x
let lwt x = Lwt x

let op_of_codec (_:'a codec): 'a op = assert false (* TODO *)

let on_start f a = On_start (f,a)
let on_done f a = On_done (f,a)

let map_l ?(j=pure int 1) f l = L_map (j,f,l)
let flat_map_l ?(j=pure int 1) f l = L_flat_map (j,f,l)

let compute op = Arg_compute op
let memo ~name codec = Arg_memo (name,codec)
let arg x a = Arg_app (x, a)

let app x f = App (x,f)

let (@>) = arg
let (|>>) x f = app x f
let (<*>) f x = app x f

let ignore_result a = a @> compute unit |>> fun _ -> ()

let fold op f acc l = acc @> l @> compute op |>> f

(** {2 Run} *)

let rec op: type a. a t -> a op = function
  | Pure (op,_) -> op
  | Catch _
  | Lwt _ -> assert false (* TODO? *)
  | If (_,b,_) -> op b
  | Seq (_,b) -> op b
  | On_start (_, b) -> op b
  | On_done (_, b) -> op b
  | L_map _ -> assert false (* TODO: replace by `par_l` scheduler combinator *)

let return x = Lwt.return (Ok x)
let (>>?=) x f =
  Lwt.bind x
    (function
      | Error e -> Lwt.return (Error e)
      | Ok x -> f x)

let run v: _ or_error Lwt.t =
  let open Lwt.Infix in
  let rec run
    : type a. a t -> a or_error Lwt.t
    = function
      | Pure (_,x) -> return x
      | Catch x ->
        begin
          run x >>?= function
          | Ok x -> return x
          | Error e -> Lwt.return (Error e)
        end
      | Lwt x -> run x >>?= fun y -> y >|= CCResult.return
      | App (x, f) -> app_fun f x
      | If (a,b,c) ->
        begin
          run a >>?= function
          | true -> run b
          | false -> run c
        end
      | Seq (a,b) ->
        run a >>?= fun _ -> run b
      | On_start _ -> assert false (* TODO *)
      | On_done _ -> assert false (* TODO *)
      | L_map _ -> assert false (* TODO: actually, delegate to custom scheduler functions *)
      | L_flat_map _ -> assert false (* TODO *)
  and app_fun
    : type f ret. f -> (f,ret) args -> ret or_error Lwt.t
    = fun f args -> match args with
      | Arg_compute _ -> Lwt.return (Ok f)
      | Arg_memo _ -> assert false (* TODO *)
      | Arg_app (x,tail) ->
        run x >>?= fun y -> app_fun (f y) tail
  in
  run v

let run_exn v =
  let open Lwt.Infix in
  run v >>= function
  | Result.Ok x -> Lwt.return x
  | Result.Error e -> Lwt.fail e
