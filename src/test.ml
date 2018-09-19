
let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  assert(
    Oranger.RF.(train ~debug:true ~nprocs:4 Classification 500
                  "data/iris.txt" "Species" "ranger_model.rf"))

let () = main ()
