open! Core_kernel
open Virtual_dom
open Zelkova

module Model = struct
  type t = int
end

module Action = struct
  type t =
    | Increment
    | Decrement
end

let initial_model = 0

let update : Model.t -> Action.t -> Model.t =
 fun current action ->
  match action with
  | Increment -> current + 1
  | Decrement -> current - 1
;;

let view model ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let open Node in
  div
    []
    [ text (sprintf "Counter: %d" model)
    ; button [ Attr.on_click (fun _ -> inject Increment) ] [ text "+" ]
    ; button [ Attr.on_click (fun _ -> inject Decrement) ] [ text "-" ]
    ]
;;

let () =
  let app = App.create ~initial_model ~update ~view in
  App.start app
;;
