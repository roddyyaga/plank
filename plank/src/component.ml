open! Core_kernel
open! Import

type ('model, 'action, 'output) state_machine =
  { initialize : 'model
  ; update : 'model -> 'action -> 'model
  ; output : 'model -> 'output
  ; view : 'model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t
  }

type (_, _) t =
  | State_machine : ('model, 'action, 'output) state_machine -> ('output, Vdom.Node.t) t
  | Map : ('a, 'view_a) t * ('a * 'view_a -> 'b * 'view_b) -> ('b, 'view_b) t
  | Both : ('a, 'view_a) t * ('b, 'view_b) t -> ('a * 'b, 'view_a * 'view_b) t
  | Switch :
      ('a, 'view_a) t * ('a, ('b, 'view_b) t, 'cmp, 'enum) Total_map.t
      -> ('b, 'view_b) t

let state_machine ~initialize ~update ~output ~view =
  State_machine { initialize; update; output; view }
;;

let map t ~f = Map (t, f)
let switch on cases = Switch (on, cases)
let both a b = Both (a, b)

module Compiled = struct
  type ('model, 'action, 'output, 'view) t =
    { initial_model : 'model
    ; update : 'model -> 'action -> 'model
    ; output_and_view : 'model -> inject:('action -> Vdom.Event.t) -> 'output * 'view
    }

  type ('output, 'view) packed =
    | T : ('model, 'action, 'output, 'view) t -> ('output, 'view) packed
end

let rec compile : type a view. (a, view) t -> (a, view) Compiled.packed = function
  | State_machine { initialize; update; output; view } ->
    let output_and_view model ~inject =
      let view = view model ~inject in
      let output = output model in
      output, view
    in
    T { Compiled.initial_model = initialize; update; output_and_view }
  | Both (a_t, b_t) ->
    let (T a) = compile a_t in
    let (T b) = compile b_t in
    let initial_model = a.initial_model, b.initial_model in
    let update (a_model, b_model) = function
      | `A a_action -> a.update a_model a_action, b_model
      | `B b_action -> a_model, b.update b_model b_action
    in
    let output_and_view (a_model, b_model) ~inject =
      let a_inject a_action = inject (`A a_action) in
      let b_inject b_action = inject (`B b_action) in
      let a_output, a_view = a.output_and_view a_model ~inject:a_inject in
      let b_output, b_view = b.output_and_view b_model ~inject:b_inject in
      (a_output, b_output), (a_view, b_view)
    in
    T { Compiled.initial_model; update; output_and_view }
  | Map (a_t, f) ->
    let (T t) = compile a_t in
    let initial_model = t.initial_model in
    let update = t.update in
    let output_and_view model ~inject =
      let output, view = t.output_and_view model ~inject in
      f (output, view)
    in
    T { Compiled.initial_model; update; output_and_view }
;;

let to_app : type a. (a, Vdom.Node.t) t -> Zelkova.App.packed =
  let open Zelkova in
  fun t ->
    let (T { Compiled.initial_model; update; output_and_view }) = compile t in
    let view model ~inject =
      let _output, view = output_and_view model ~inject in
      view
    in
    T (App.create ~initial_model ~update ~view)
;;

module Let_syntax = struct
  module Let_syntax = struct
    let map = map

    (*$
      open! Core_kernel

      let a i = sprintf "a_%d" i
      let view i = sprintf "view_%d" i

      let rec boths = function
        | 2 -> sprintf "both %s %s" (a 0) (a 1)
        | n -> sprintf "both (%s) %s" (boths (n - 1)) (a (n - 1))
      ;;

      let rec nested_tuple make = function
        | 2 -> sprintf "(%s, %s)" (make 0) (make 1)
        | n -> sprintf "(%s, %s)" (nested_tuple make (n - 1)) (make (n - 1))
      ;;

      let write_mapn n =
        printf "let map%d %s ~f =\n" n (String.concat ~sep:" " (List.init n ~f:a));
        print_endline (boths n);
        let outputs = nested_tuple a n in
        let views = nested_tuple view n in
        let paired =
          String.concat
            ~sep:" "
            (List.init n ~f:(fun n -> sprintf "(%s, %s)" (a n) (view n)))
        in
        printf "|> map ~f:(fun (%s, %s) -> f %s)\n" outputs views paired
      ;;

      let () =
        List.init 10 ~f:Fn.id |> List.tl_exn |> List.tl_exn |> List.iter ~f:write_mapn
      ;;
    *)
    let map2 a_0 a_1 ~f =
      both a_0 a_1
      |> map ~f:(fun ((a_0, a_1), (view_0, view_1)) -> f (a_0, view_0) (a_1, view_1))
    ;;

    let map3 a_0 a_1 a_2 ~f =
      both (both a_0 a_1) a_2
      |> map ~f:(fun (((a_0, a_1), a_2), ((view_0, view_1), view_2)) ->
             f (a_0, view_0) (a_1, view_1) (a_2, view_2))
    ;;

    let map4 a_0 a_1 a_2 a_3 ~f =
      both (both (both a_0 a_1) a_2) a_3
      |> map ~f:(fun ((((a_0, a_1), a_2), a_3), (((view_0, view_1), view_2), view_3)) ->
             f (a_0, view_0) (a_1, view_1) (a_2, view_2) (a_3, view_3))
    ;;

    let map5 a_0 a_1 a_2 a_3 a_4 ~f =
      both (both (both (both a_0 a_1) a_2) a_3) a_4
      |> map
           ~f:(fun
                ( ((((a_0, a_1), a_2), a_3), a_4)
                , ((((view_0, view_1), view_2), view_3), view_4) )
              -> f (a_0, view_0) (a_1, view_1) (a_2, view_2) (a_3, view_3) (a_4, view_4))
    ;;

    let map6 a_0 a_1 a_2 a_3 a_4 a_5 ~f =
      both (both (both (both (both a_0 a_1) a_2) a_3) a_4) a_5
      |> map
           ~f:(fun
                ( (((((a_0, a_1), a_2), a_3), a_4), a_5)
                , (((((view_0, view_1), view_2), view_3), view_4), view_5) )
              ->
             f
               (a_0, view_0)
               (a_1, view_1)
               (a_2, view_2)
               (a_3, view_3)
               (a_4, view_4)
               (a_5, view_5))
    ;;

    let map7 a_0 a_1 a_2 a_3 a_4 a_5 a_6 ~f =
      both (both (both (both (both (both a_0 a_1) a_2) a_3) a_4) a_5) a_6
      |> map
           ~f:(fun
                ( ((((((a_0, a_1), a_2), a_3), a_4), a_5), a_6)
                , ((((((view_0, view_1), view_2), view_3), view_4), view_5), view_6) )
              ->
             f
               (a_0, view_0)
               (a_1, view_1)
               (a_2, view_2)
               (a_3, view_3)
               (a_4, view_4)
               (a_5, view_5)
               (a_6, view_6))
    ;;

    let map8 a_0 a_1 a_2 a_3 a_4 a_5 a_6 a_7 ~f =
      both (both (both (both (both (both (both a_0 a_1) a_2) a_3) a_4) a_5) a_6) a_7
      |> map
           ~f:(fun
                ( (((((((a_0, a_1), a_2), a_3), a_4), a_5), a_6), a_7)
                , ( ((((((view_0, view_1), view_2), view_3), view_4), view_5), view_6)
                  , view_7 ) )
              ->
             f
               (a_0, view_0)
               (a_1, view_1)
               (a_2, view_2)
               (a_3, view_3)
               (a_4, view_4)
               (a_5, view_5)
               (a_6, view_6)
               (a_7, view_7))
    ;;

    let map9 a_0 a_1 a_2 a_3 a_4 a_5 a_6 a_7 a_8 ~f =
      both
        (both (both (both (both (both (both (both a_0 a_1) a_2) a_3) a_4) a_5) a_6) a_7)
        a_8
      |> map
           ~f:(fun
                ( ((((((((a_0, a_1), a_2), a_3), a_4), a_5), a_6), a_7), a_8)
                , ( ( ((((((view_0, view_1), view_2), view_3), view_4), view_5), view_6)
                    , view_7 )
                  , view_8 ) )
              ->
             f
               (a_0, view_0)
               (a_1, view_1)
               (a_2, view_2)
               (a_3, view_3)
               (a_4, view_4)
               (a_5, view_5)
               (a_6, view_6)
               (a_7, view_7)
               (a_8, view_8))
    ;;
    (*$*)
  end
end

let counter =
  let update x = function
    | `Incr -> x + 1
    | `Decr -> x - 1
  in
  let view model ~inject =
    let open Vdom in
    let open Node in
    div
      []
      [ text (sprintf "Counter: %d" model)
      ; button [ Attr.on_click (fun _ -> inject `Incr) ] [ text "+" ]
      ; button [ Attr.on_click (fun _ -> inject `Decr) ] [ text "-" ]
      ]
  in
  state_machine ~initialize:0 ~update ~output:Fn.id ~view
;;

let _sum =
  both counter counter
  |> map ~f:(fun ((a, b), (a_view, b_view)) -> a + b, Vdom.Node.div [] [ a_view; b_view ])
;;

let _incred =
  let open Let_syntax in
  let%mapn x, view = counter in
  x + 1, Vdom.Node.(div [] [ text "hallo"; view ])
;;

let _sum' =
  let open Let_syntax in
  let%mapn x, x_view = counter
  and y, y_view = counter
  and _z, _z_view = counter in
  x + y, Vdom.Node.(div [] [ text "hallo"; x_view; y_view ])
;;

module Enum = struct
  module T = struct
    type t =
      | Foo
      | Bar
      | Baz
    [@@deriving bin_io, compare, enumerate, sexp]
  end

  include T
  module Total_map = Total_map.Make (T)
end

(*let _switch_example =*)
(*let enum_choice = assert false in*)
(*let foo_form = assert false in*)
(*let bar_form = assert false in*)
(*let baz_form = assert false in*)
(*let cases =*)
(*Enum.Total_map.create (function*)
(*| Foo -> foo_form*)
(*| Bar -> bar_form*)
(*| Baz -> baz_form)*)
(*in*)
(*let open Let_syntax in*)
(*let%mapn choice, choice_view = enum_choice*)
(*and value, value_view = switch enum_choice cases in*)
(*(choice, value), [ choice_view; value_view ]*)
(*;;*)
