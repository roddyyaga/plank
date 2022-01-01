open! Core_kernel
open! Import

type ('model, 'action, 'output) state_machine =
  { initialize : 'model
  ; update : 'model -> 'action -> 'model
  ; output : 'model -> 'output
  ; view : 'model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t
  }

type (_, _) t =
  | Return : ('output * 'view) -> ('output, 'view) t
  | State_machine :
      ('model, 'action, 'output) state_machine
      -> ('output, ('action -> Vdom.Event.t) * Vdom.Node.t) t
  | Map : ('a, 'view_a, 'b, 'view_b) map -> ('b, 'view_b) t
  | Both : ('a, 'view_a) t * ('b, 'view_b) t -> ('a * 'b, 'view_a * 'view_b) t
  | Switch :
      ('a, 'view_a) t * ('a, ('b, 'view_a -> 'view_b) t, 'cmp, 'enum) Total_map.t
      -> ('b, 'view_b) t

and ('a, 'view_a, 'b, 'view_b) map =
  { t : ('a, 'view_a) t
  ; output : 'a -> 'b
  ; view : 'a -> 'view_a -> 'view_b
  }

let return output view = Return (output, view)
let view view = Return ((), view)

let state_machine ~initialize ~update ~output ~view =
  State_machine { initialize; update; output; view }
;;

let no_output _ = ()

let state ~initial:initialize ~view =
  state_machine ~initialize ~update:(fun _ latest -> latest) ~output:Fn.id ~view
;;

let map t ~output ~view = Map { t; output; view }
let both a b = Both (a, b)

let map2 a_t b_t ~output ~view =
  both a_t b_t |> map ~output:(fun (a, b) -> output a b) ~view
;;

let all ts =
  List.fold ts ~init:(return [] []) ~f:(fun acc t ->
      both t acc |> map ~output:(fun (x, xs) -> x :: xs) ~view:(fun _ (x, xs) -> x :: xs))
;;

let all_map t_map =
  let empty = Map.empty (Map.comparator_s t_map) in
  Map.fold t_map ~init:(return empty empty) ~f:(fun ~key ~data acc ->
      both data acc
      |> map
           ~output:(fun (data, acc) -> Map.add_exn acc ~key ~data)
           ~view:(fun _ (data, acc) -> Map.add_exn acc ~key ~data))
;;

let cap t = map t ~output:Fn.id ~view:(fun _ view -> snd view)

module Output = struct
  let map t ~f = map t ~output:f ~view:(fun _ v -> v)

  module Let_syntax = struct
    module Let_syntax = struct
      let map = map
    end
  end
end

module View = struct
  let map t ~f = map t ~output:Fn.id ~view:(fun _ v -> f v)

  module Infix = struct
    let ( >>| ) t f = map t ~f
  end

  module Let_syntax = struct
    module Let_syntax = struct
      let map = map
    end
  end
end

let switch on cases = Switch (on, cases)

module Switch = struct
  module Infix = struct end
end

module Compiled = struct
  type ('model, 'action, 'output, 'view) t =
    { initial_model : 'model
    ; update : 'model -> 'action -> 'model
    ; output : 'model -> 'output
    ; view : 'model -> inject:('action -> Vdom.Event.t) -> 'view
    }
  [@@deriving fields]

  type ('output, 'view) packed =
    | T : ('model, 'action, 'output, 'view) t -> ('output, 'view) packed

  type ('output, 'view) packed_with_model =
    | Pack_with_model :
        ('model, 'action, 'output, 'view) t * 'model
        -> ('output, 'view) packed_with_model
end

let rec compile : type a view. (a, view) t -> (a, view) Compiled.packed = function
  | Return (output, view) ->
    let update () () = () in
    let output _ = output in
    let view () ~inject:_ = view in
    T { Compiled.initial_model = (); update; output; view }
  | State_machine { initialize; update; output; view } ->
    let view model ~inject = inject, view model ~inject in
    T { Compiled.initial_model = initialize; update; output; view }
  | Both (a_t, b_t) ->
    let (T a) = compile a_t in
    let (T b) = compile b_t in
    let initial_model = a.initial_model, b.initial_model in
    let update (a_model, b_model) = function
      | `A a_action -> a.update a_model a_action, b_model
      | `B b_action -> a_model, b.update b_model b_action
    in
    let output (a_model, b_model) = a.output a_model, b.output b_model in
    let view (a_model, b_model) ~inject =
      let a_inject a_action = inject (`A a_action) in
      let b_inject b_action = inject (`B b_action) in
      let a_view = a.view a_model ~inject:a_inject in
      let b_view = b.view b_model ~inject:b_inject in
      a_view, b_view
    in
    T { Compiled.initial_model; update; output; view }
  | Map { t = a_t; output = map_output; view = map_view } ->
    let (T t) = compile a_t in
    let initial_model = t.initial_model in
    let update = t.update in
    let output model = map_output (t.output model) in
    let view model ~inject =
      let output = t.output model in
      map_view output (t.view model ~inject)
    in
    T { Compiled.initial_model; update; output; view }
  | Switch (a_t, cases) ->
    let (T a) = compile a_t in
    let compiled = Total_map.map cases ~f:compile in
    let open Compiled in
    let initial_model =
      ( a.initial_model
      , Total_map.map compiled ~f:(fun (T b) -> Pack_with_model (b, b.initial_model)) )
    in
    let update (a_model, b_models) = function
      | `Key_action action -> a.update a_model action, b_models
      | `Value_action (key, b_action) ->
        let (Pack_with_model (b, b_model)) = Total_map.find b_models key in
        (* TODO - maybe replace Obj.magic with Type_equal *)
        let b_model = b.update b_model (Obj.magic b_action) in
        a_model, Total_map.set b_models key (Pack_with_model (b, b_model))
    in
    let output (a_model, b_models) =
      let (Pack_with_model (b, b_model)) = Total_map.find b_models (a.output a_model) in
      b.output b_model
    in
    let view (a_model, b_models) ~inject =
      let a_inject key_action = inject (`Key_action key_action) in
      let a_view = a.view a_model ~inject:a_inject in
      let a_output = a.output a_model in
      let (Pack_with_model (b, b_model)) = Total_map.find b_models a_output in
      let b_inject b_action = inject (`Value_action (a_output, Obj.magic b_action)) in
      let view_function = b.view b_model ~inject:b_inject in
      view_function a_view
    in
    T { Compiled.initial_model; update; output; view }
;;

let to_app : type a. (a, Vdom.Node.t) t -> Zelkova.App.packed =
  let open Zelkova in
  fun t ->
    let (T { Compiled.initial_model; update; view; output = _ }) = compile t in
    T (App.create ~initial_model ~update ~view)
;;

let textbox =
  let view content ~inject =
    let open Vdom in
    Node.input [ Attr.on_input (fun _ content -> inject content) ] [ Node.text content ]
  in
  state ~initial:"" ~view
;;
