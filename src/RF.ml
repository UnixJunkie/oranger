
open Printf

module A = BatArray
module L = BatList

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
        "ml_rf_ranger %s --file %s --depvarname %s --treetype %d --ntree %d \
         --write --outprefix %s --nthreads %d"
        (if debug then "--verbose" else "")
        data_fn
        dep_var_name
        (int_of_mode mode)
        nb_trees
        model_out_fn
        nprocs in
    Log.debug "cmd: %s" cmd;
    let status, log = BatUnix.run_and_read cmd in
    Log.info "%s" log;
    match status with
    | WEXITED 0 ->
      (Sys.rename (model_out_fn ^ ".forest") model_out_fn;
       true)
    | _ -> false

let read_raw_class_predictions nb_trees fn =
  let status, predicted_classes_str =
    (* keep only integer lines *)
    BatUnix.run_and_read
      (sprintf "awk '/^[0-9]+$/{print $0}' %s" fn) in
  assert(status = WEXITED 0);
  let pred_strings =
    BatString.nsplit ~by:"\n" (BatString.strip predicted_classes_str) in
  let nb_preds = L.length pred_strings in
  Log.debug "nb integer preds: %d" (L.length pred_strings);
  let pred_classes = L.map float_of_string pred_strings in
  let nb_samples = nb_preds / nb_trees in
  Log.debug "nb samples: %d" nb_samples;
  let preds = A.of_list pred_classes in
  let res = ref [] in
  for samp_i = 0 to nb_samples - 1 do
    (* gather class predictions for this sample *)
    let curr_preds = ref [] in
    for tree_j = 0 to nb_trees - 1 do
      let offset = (tree_j * nb_samples) + samp_i in
      curr_preds := preds.(offset) :: !curr_preds
    done;
    (* compute mean and stddev *)
    let avg = L.favg !curr_preds in
    let std = Utls.stddev !curr_preds in
    res := (avg, std) :: !res
  done;
  L.rev !res

let classify
    ?debug:(debug = false)
    ?nprocs:(nprocs = 1)
    (nb_trees: int)
    (data_fn: filename)
    (model_fn: filename): (float * float) list option =
  let predictions_fn = Filename.temp_file "oranger_" "" in
  let cmd =
    sprintf
      "ml_rf_ranger %s \
       --file %s --predict %s --nthreads %d --outprefix %s --predall"
      (if debug then "--verbose" else "")
      data_fn
      model_fn
      nprocs
      predictions_fn in
  Log.debug "cmd: %s" cmd;
  let status, log = BatUnix.run_and_read cmd in
  Log.info "%s" log;
  match status with
  | WEXITED 0 ->
    begin
      let raw_preds_fn = predictions_fn ^ ".prediction" in
      Some (read_raw_class_predictions nb_trees raw_preds_fn)
    end
  | _ -> None
