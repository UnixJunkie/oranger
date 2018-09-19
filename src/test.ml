open Printf

module L = BatList

let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  assert(
    Oranger.RF.(train ~debug:true Classification 500
                  "data/iris.txt" "Species" "ranger_model.rf"));
  let preds =
    Oranger.RF.classify ~debug:true 500 "data/iris.txt" "ranger_model.rf" in
  match preds with
  | None -> assert(false)
  | Some xs ->
    L.iter (fun (mean, stddev) ->
        printf "%f %f\n" mean stddev
      ) xs

let () = main ()
