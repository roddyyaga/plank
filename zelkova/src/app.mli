open! Core_kernel
open! Import

type ('model, 'action) t

val create
  :  initial_model:'model
  -> update:('model -> 'action -> 'model)
  -> view:('model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t)
  -> ('model, 'action) t

val view : (_, _) t -> Vdom.Node.t
val state : ('model, 'action) t -> 'model
val start : (_, _) t -> unit

type packed = T : (_, _) t -> packed
