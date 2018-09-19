
open Printf

type filename = string

type mode = Regression
          | Classification

let int_of_mode = function
  | Classification -> 1
  | Regression -> 3

let train
    ?debug:(debug = false)
    ?nprocs:(nprocs = 1)
    (mode: mode)
    (nb_trees: int)
    (data_fn: filename)
    (dep_var_name: string)
    (model_out_fn: filename): bool =
  match mode with
  | Regression -> failwith "not implemented yet"
  | Classification ->
    let cmd =
      sprintf
        "ranger %s --file %s --depvarname %s --treetype %d --ntree %d \
         --write %s --nthreads %d --predall"
        (if debug then "--verbose" else "")
        data_fn
        dep_var_name
        (int_of_mode mode)
        nb_trees
        model_out_fn
        nprocs in
    let status, log = BatUnix.run_and_read cmd in
    Log.info "%s" log;
    match status with
    | WEXITED 0 -> true
    | _ -> false
