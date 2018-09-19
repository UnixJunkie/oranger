open Printf

module L = List

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

(* population standard deviation *)
let stddev (l: float list): float =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)
