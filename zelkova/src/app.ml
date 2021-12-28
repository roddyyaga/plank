open! Core_kernel
open! Import

type 'model state =
  { mutable model : 'model
  ; mutable current_view : Vdom.Node.t
  ; mutable on_update : (previous:Vdom.Node.t -> current:Vdom.Node.t -> unit) option
  }

type ('model, 'action) t =
  { update : 'model -> 'action -> 'model
  ; view : 'model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t
  ; state : 'model state
  }

let view t = t.state.current_view
let state t = t.state.model

let create (type action) ~initial_model ~(update : 'model -> action -> 'model) ~view =
  (* We define [inject] as a ref to a no-op function,
      get a new inject function to pass to [view] that calls the function at this ref,
      then redefine [inject] to be the real implementation.

      This seems like the least nasty way of getting round the circular dependency. *)
  let inject = ref (fun _action -> ()) in
  let module Handler = struct
    module Action = struct
      type t = action
    end

    let handle action = !inject action
  end
  in
  let module Event = Vdom.Event.Define (Handler) in
  let state =
    { model = initial_model
    ; current_view = view initial_model ~inject:Event.inject
    ; on_update = None
    }
  in
  let () =
    let real_inject action =
      let model = update state.model action in
      let new_current_view = view model ~inject:Event.inject in
      let () =
        Option.iter state.on_update ~f:(fun f ->
            f ~previous:state.current_view ~current:new_current_view)
      in
      state.model <- model;
      state.current_view <- new_current_view
    in
    inject := real_inject
  in
  let t = { update; view; state } in
  t
;;

let start t =
  let module Node = Vdom.Node in
  Dom_html.window##.onload
    := Dom.handler (fun _ ->
           let elt = Node.to_dom t.state.current_view in
           Dom.appendChild Dom_html.document##.body elt;
           let on_update ~previous ~current =
             let patch = Node.Patch.create ~previous ~current in
             let (_ : Dom_html.element Js.t) = Node.Patch.apply patch elt in
             ()
           in
           t.state.on_update <- Some on_update;
           Js._false)
;;

type packed = T : (_, _) t -> packed
