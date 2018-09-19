open Printf

module L = BatList

let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  let nb_trees = 500 in
  assert(
    Oranger.RF.(train ~debug:true Classification nb_trees
                  "data/iris.txt" "Species" "ranger_model.rf"));
  let preds =
    Oranger.RF.classify ~debug:true nb_trees "data/iris.txt" "ranger_model.rf" in
  match preds with
  | None -> assert(false)
  | Some xs ->
    L.iter (fun (mean, stddev) ->
        printf "%f %f\n" mean stddev
      ) xs

let () = main ()
