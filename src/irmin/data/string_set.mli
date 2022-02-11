type t
type elt := string

val create :
  initial_capacity:int ->
  elt_length:int ->
  hash:(elt -> int) ->
  hash_substring:(Bigstringaf.t -> off:int -> len:int -> int) ->
  unit ->
  t

val add : t -> elt -> unit
val mem : t -> elt -> bool
val invariant : (elt -> unit) -> t -> unit
val reachable_words : t -> int
