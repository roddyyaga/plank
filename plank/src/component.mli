open! Core_kernel
open! Import

type ('output, 'view) t

val state_machine
  :  initialize:'model
  -> update:('model -> 'action -> 'model)
  -> output:('model -> 'output)
  -> view:('model -> inject:('action -> Vdom.Event.t) -> Vdom.Node.t)
  -> ('output, Vdom.Node.t) t

val map : ('a, 'view_a) t -> f:('a * 'view_a -> 'b * 'view_b) -> ('b, 'view_b) t
val both : ('a, 'view_a) t -> ('b, 'view_b) t -> ('a * 'b, 'view_a * 'view_b) t
val to_app : (_, Vdom.Node.t) t -> Zelkova.App.packed

val switch
  :  ('a, 'view_a) t
  -> ('a, ('b, 'view_b) t, 'cmp, 'enum) Total_map.t
  -> ('b, 'view_b) t

module Let_syntax : sig
  module Let_syntax : sig
    val map : ('a, 'view_a) t -> f:('a * 'view_a -> 'b * 'view_b) -> ('b, 'view_b) t

    (*$
      open! Core_kernel

      let a i = sprintf "'a_%d" i
      let view i = sprintf "'v_%d" i

      let write_mapn n =
        printf "val map%d :\n" n;
        let n = n + 1 in
        let args =
          List.init (n - 1) ~f:(fun i -> sprintf "(%s, %s) t" (a i) (view i))
          |> String.concat ~sep:" -> "
        in
        print_endline args;
        let f_type =
          List.init n ~f:(fun i -> sprintf "%s * %s" (a i) (view i))
          |> String.concat ~sep:" -> "
        in
        printf "-> f:(%s)\n" f_type;
        printf "-> (%s, %s) t\n" (a (n - 1)) (view (n - 1))
      ;;

      let () =
        List.init 10 ~f:Fn.id |> List.tl_exn |> List.tl_exn |> List.iter ~f:write_mapn
      ;;
    *)val map2 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2)
-> ('a_2, 'v_2) t
val map3 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3)
-> ('a_3, 'v_3) t
val map4 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4)
-> ('a_4, 'v_4) t
val map5 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t -> ('a_4, 'v_4) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4 -> 'a_5 * 'v_5)
-> ('a_5, 'v_5) t
val map6 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t -> ('a_4, 'v_4) t -> ('a_5, 'v_5) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4 -> 'a_5 * 'v_5 -> 'a_6 * 'v_6)
-> ('a_6, 'v_6) t
val map7 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t -> ('a_4, 'v_4) t -> ('a_5, 'v_5) t -> ('a_6, 'v_6) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4 -> 'a_5 * 'v_5 -> 'a_6 * 'v_6 -> 'a_7 * 'v_7)
-> ('a_7, 'v_7) t
val map8 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t -> ('a_4, 'v_4) t -> ('a_5, 'v_5) t -> ('a_6, 'v_6) t -> ('a_7, 'v_7) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4 -> 'a_5 * 'v_5 -> 'a_6 * 'v_6 -> 'a_7 * 'v_7 -> 'a_8 * 'v_8)
-> ('a_8, 'v_8) t
val map9 :
('a_0, 'v_0) t -> ('a_1, 'v_1) t -> ('a_2, 'v_2) t -> ('a_3, 'v_3) t -> ('a_4, 'v_4) t -> ('a_5, 'v_5) t -> ('a_6, 'v_6) t -> ('a_7, 'v_7) t -> ('a_8, 'v_8) t
-> f:('a_0 * 'v_0 -> 'a_1 * 'v_1 -> 'a_2 * 'v_2 -> 'a_3 * 'v_3 -> 'a_4 * 'v_4 -> 'a_5 * 'v_5 -> 'a_6 * 'v_6 -> 'a_7 * 'v_7 -> 'a_8 * 'v_8 -> 'a_9 * 'v_9)
-> ('a_9, 'v_9) t
(*$*)
  end
end
