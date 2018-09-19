open Printf

module L = BatList

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = Cpm.MakeROC.Make(Score_label)


let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  let nb_trees = 500 in
  let class_label_field = "1832" in
  assert(
    Oranger.RF.(train ~debug:true Classification nb_trees
                  "data/train.txt" class_label_field "ranger_model.rf"));
  let preds =
    Oranger.RF.classify ~debug:true nb_trees "data/test.txt" "ranger_model.rf" in
  match preds with
  | None -> assert(false)
  | Some score_stddevs ->
    (* L.iter (fun (mean, stddev) ->
     *     printf "%f %f\n" mean stddev
     *   ) xs *)
    let labels = [true; true; true; true; true;
                  false; false; false; false; false] in
    let scores = L.map fst score_stddevs in
    let score_labels = L.combine labels scores in
    let auc = ROC.auc score_labels in
    printf "AUC: %.3f\n" auc

let () = main ()
