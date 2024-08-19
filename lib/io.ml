let file_read f =
  let ic = open_in f in
  try
    let rec read_lines () =
      try
        let line = input_line ic in
        line :: read_lines ()
      with _ -> []
    in
    let lines = read_lines () in
    close_in ic;
    lines
  with e ->
    close_in_noerr ic;
    raise e

let rec file_fold_left_lines' ic cb acc =
  try
    let line = input_line ic in
    file_fold_left_lines' ic cb (cb line acc)
  with _ -> acc

let file_fold_left_lines f cb acc =
  let ic = open_in f in
  try
    let ret = file_fold_left_lines' ic cb acc in
    close_in ic;
    ret
  with e ->
    close_in_noerr ic;
    raise e
