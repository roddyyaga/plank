open! Core_kernel
open Virtual_dom
open Plank

let need_odd_message =
  view Vdom.Node.(div [] [ text "Set the counter to an odd number!" ])
;;

module Todo = struct
  type t =
    { title : string
    ; completed : bool
    ; editing : bool
    }

  let plank ~initial_title =
    let view ({ title; completed; editing } as t) ~inject =
      let open Vdom in
      let class_if b c = if b then [ Attr.class_ c ] else [] in
      Node.li
        (class_if editing "editing" @ class_if completed "completed")
        [ Node.div
            [ Attr.class_ "view" ]
            [ Node.input
                ([ Attr.id "toggle"; Attr.type_ "checkbox" ]
                @ if completed then [ Attr.checked ] else [])
                []
            ; Node.label
                [ Attr.on_double_click (fun _ -> inject { t with editing = true }) ]
                [ Node.text title ]
            ; Node.button [ Attr.class_ "destroy" ] []
            ]
        ; Node.input [ Attr.class_ "edit" ] []
        ]
    in
    state ~initial:{ title = initial_title; completed = false; editing = false } ~view
  ;;
end

type a = Add of string

let todos_plank =
  let initialize = [] in
  let update todos = function
    | Add initial_title -> todos @ [ Todo.plank ~initial_title ]
  in
  let view _ ~inject:_ = Vdom.Node.div [] [] in
  let output = Fn.id in
  state_machine ~initialize ~update ~output ~view
;;

let main todos =
  todos
  |> all
  |> map ~output:no_output ~view:(fun _ todo_views ->
         let open Vdom in
         let toggle_all = "toggle-all" in
         Node.section
           [ Attr.class_ "main" ]
           [ Node.input
               [ Attr.id toggle_all; Attr.class_ toggle_all; Attr.type_ "checkbox" ]
               []
           ; Node.label [ Attr.for_ toggle_all ] [ Node.text "Mark all as complete" ]
           ; Node.ul [ Attr.class_ "todo-list" ] todo_views
           ])
;;

(*let main todos =*)
(*let update todos = function*)
(*| `Add initial_title -> todos @ [ Todo.plank ~initial_title ]*)
(*in*)
(*let view todos ~inject:_ =*)
(*let open Vdom in*)
(*let toggle_all = "toggle-all" in*)
(*Node.section*)
(*[ Attr.class_ "main" ]*)
(*[ Node.input*)
(*[ Attr.id toggle_all; Attr.class_ toggle_all; Attr.type_ "checkbox" ]*)
(*[]*)
(*; Node.label [ Attr.for_ toggle_all ] [ Node.text "Mark all as complete" ]*)
(*; Node.ul [ Attr.class_ "todo-list" ] []*)
(*]*)
(*in*)
(*state_machine ~initialize:[] ~update ~output:no_output ~view*)
(*;;*)

let app = need_odd_message

let () =
  let (T app) = to_app app in
  Zelkova.App.start app
;;
