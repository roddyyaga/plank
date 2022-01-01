open! Core_kernel
open! Import

type ('output, 'view) t

val to_app : (unit, Vdom.Node.t) t -> Zelkova.App.packed

val state_machine
  :  initialize:'model
  -> update:('model -> 'action -> 'model)
  -> output:('model -> 'output)
  -> view:('model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t)
  -> ('output, ('action -> Vdom.Event.t) * Vdom.Node.t) t

val no_output : _ -> unit
val cap : ('a, _ * Vdom.Node.t) t -> ('a, Vdom.Node.t) t

val state
  :  initial:'a
  -> view:('a -> inject:('a -> Vdom.Event.t) -> Vdom.Node.t)
  -> ('a, ('a -> Vdom.Event.t) * Vdom.Node.t) t

val return : 'output -> 'view -> ('output, 'view) t
val view : 'view -> (unit, 'view) t

(** In type theory jargon, [('output, 'view) t] is a special kind of biapplicative.
 * This definition of [map] is stronger than the normal definition because of the extra parameter to [view]. Combined with the regular versions of [return] and [both],
 * we can define [apply] as in the standard signature (https://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Biapplicative.html). We don't define it here because it seems unlikely to be useful.
 *)
val map
  :  ('a, 'view_a) t
  -> output:('a -> 'b)
  -> view:('a -> 'view_a -> 'view_b)
  -> ('b, 'view_b) t

val both : ('a, 'view_a) t -> ('b, 'view_b) t -> ('a * 'b, 'view_a * 'view_b) t

val map2
  :  ('a, 'v_a) t
  -> ('b, 'v_b) t
  -> output:('a -> 'b -> 'c)
  -> view:('a * 'b -> 'v_a * 'v_b -> 'v_c)
  -> ('c, 'v_c) t

val all : ('a, 'view) t list -> ('a list, 'view list) t

val all_map
  :  ('k, ('a, 'view) t, 'cmp) Map.t
  -> (('k, 'a, 'cmp) Map.t, ('k, 'view, 'cmp) Map.t) t

module Output : sig
  val map : ('a, 'v) t -> f:('a -> 'b) -> ('b, 'v) t

  module Let_syntax : sig
    module Let_syntax : sig
      val map : ('a, 'v) t -> f:('a -> 'b) -> ('b, 'v) t
    end
  end
end

module View : sig
  val map : ('o, 'v1) t -> f:('v1 -> 'v2) -> ('o, 'v2) t

  module Infix : sig
    val ( >>| ) : ('o, 'v1) t -> ('v1 -> 'v2) -> ('o, 'v2) t
  end

  module Let_syntax : sig
    module Let_syntax : sig
      val map : ('o, 'v1) t -> f:('v1 -> 'v2) -> ('o, 'v2) t
    end
  end
end

val switch
  :  ('a, 'view_a) t
  -> ('a, ('b, 'view_a -> 'view_b) t, 'cmp, 'enum) Total_map.t
  -> ('b, 'view_b) t

(*val list*)
(*:  ('a, 'view) t list*)
(*-> (unit, 'view list * ([ `Add of ('a, 'view) t ] -> Vdom.Event.t)) t*)

module Switch : sig
  module Infix : sig end
end

val textbox : (string, (string -> Vdom.Event.t) * Vdom.Node.t) t
