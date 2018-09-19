open Printf

module L = List

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let ignore_fst _fst snd =
  snd

let fold_on_lines_of_file fn f acc =
  with_in_file fn (fun input ->
      let acc' = ref acc in
      try
        while true do
          acc' := f !acc' (input_line input)
        done;
        assert(false)
      with End_of_file -> !acc'
    )

let float_list_of_file fn =
  let res =
    fold_on_lines_of_file fn (fun acc line ->
        let pred =
          try Scanf.sscanf line "%f" (fun x -> x)
          with Scanf.Scan_failure msg ->
            (* percolate a NaN rather than crashing *)
            (Log.error "%s: %s" msg line;
             nan) in
        pred :: acc
      ) [] in
  L.rev res
