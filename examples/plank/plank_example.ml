open! Core_kernel
open Virtual_dom
open Plank

let no_output _ = ()

module Counter = struct
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

  let plank = state_machine ~initialize:0 ~update ~view ~output:Fn.id |> cap
end

module Parity = struct
  module T = struct
    type t =
      | Even
      | Odd
    [@@deriving bin_io, compare, enumerate, sexp]
  end

  include T
  module Total_map = Total_map.Make (T)

  let plank = Output.map Counter.plank ~f:(fun i -> if i % 2 = 0 then Even else Odd)
end

let need_odd_message =
  view Vdom.Node.(div [] [ text "Set the counter to an odd number!" ])
;;

let app =
  let text_if_odd =
    let even_view =
      View.map need_odd_message ~f:(fun msg parity_view ->
          Vdom.Node.div [] [ parity_view; msg ])
      |> Output.map ~f:(fun () -> None)
    in
    let odd_view =
      View.map (cap textbox) ~f:(fun textbox parity_view ->
          Vdom.Node.div [] [ textbox; parity_view ])
      |> Output.map ~f:Option.return
    in
    switch
      Parity.plank
      (Parity.Total_map.create (function
          | Even -> even_view
          | Odd -> odd_view))
  in
  map
    text_if_odd
    ~view:(fun state form ->
      Vdom.Node.(
        div
          []
          [ div [] [ text (state |> [%sexp_of: string option] |> Sexp.to_string_hum) ]
          ; form
          ]))
    ~output:(Fn.const ())
;;

let () =
  let (T app) = to_app app in
  Zelkova.App.start app
;;
