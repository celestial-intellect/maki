
(** {1 Maki} *)

type 'a or_error = ('a, exn) CCResult.t

type -'a op

val make_op : 'a CCFormat.printer -> ('a -> Sha1.t) -> 'a op

val unit : unit op
val int : int op
val bool : bool op
val option : 'a op -> 'a option op
val list : 'a op -> 'a list op
val list_set : 'a op -> 'a list op (** Unordered *)

type 'a codec = {
  encode: 'a -> string;
  decode: string -> 'a or_error;
}

type 'a t
(** Computations returning a ['a] *)

val pure : 'a op -> 'a -> 'a t

val if_ : bool t -> 'a t -> 'a t -> 'a t

val catch : 'a or_error t -> 'a t

val lwt : 'a Lwt.t t -> 'a t

val ignore_result : _ t -> unit t

val (>>) : unit t -> 'a t -> 'a t

val seq : unit t -> 'a t -> 'a t

val on_done : ('a or_error -> unit) -> 'a t -> 'a t

val on_start : (unit -> unit) -> 'a t -> 'a t

(** {2 Functions} *)

(** A function takes a tuple of arguments, all of which have type [_ t],
    and returns a [_ t] too.
    Some functions can memoize their result given the arguments. *)

type ('a, 'ret) args

val arg : 'a t -> ('b, 'ret) args -> ('a -> 'b, 'ret) args

val (@>) : 'a t -> ('b, 'ret) args -> ('a -> 'b, 'ret) args
(** Alias to {!arg} *)

val compute : 'ret op -> ('ret, 'ret) args

val memo : name:string -> 'ret codec -> (unit -> 'ret, 'ret) args

val app : ('f, 'ret) args -> 'f -> 'ret t

val (|>>) : ('f, 'ret) args -> 'f -> 'ret t

val (<*>) : 'f -> ('f, 'ret) args -> 'ret t

(** {2 Helpers} *)

val map_l : ('a -> 'b t) -> 'a list t -> 'b t list t

val flat_map_l : ('a -> 'b t list t) t -> 'a list t -> 'b t list t

val fold : 'a op -> ('a -> 'b -> 'a) -> 'a t -> 'b list t -> 'a t

val par_l : ?j:int t -> 'a t list t -> 'a list t
(** Execute the computations in the list with [j] parallel futures at
    the same time *)

(** {2 Running} *)

val run : 'a t -> 'a or_error Lwt.t

val run_exn : 'a t -> 'a Lwt.t
