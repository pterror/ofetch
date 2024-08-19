let si_prefixes = " kMGTPEZYRQ"
let b (n : int) = string_of_int n ^ " B"
let kib (n : int) = string_of_float (float_of_int n /. 1_024.0) ^ " kiB"
let mib (n : int) = string_of_float (float_of_int n /. 1_048_576.0) ^ " MiB"
let gib (n : int) = string_of_float (float_of_int n /. 1_073_741_824.0) ^ " GiB"

let tib (n : int) =
  string_of_float (float_of_int n /. 1_099_511_627_776.0) ^ " TiB"

let eib (n : int) =
  string_of_float (float_of_int n /. 1_125_899_906_842_624.0) ^ " EiB"

let kb (n : int) = string_of_float (float_of_int n /. 1_000.0) ^ " kB"
let mb (n : int) = string_of_float (float_of_int n /. 1_000_000.0) ^ " MB"
let gb (n : int) = string_of_float (float_of_int n /. 1_000_000_000.0) ^ " GB"

let tb (n : int) =
  string_of_float (float_of_int n /. 1_000_000_000_000.0) ^ " TB"

let eb (n : int) =
  string_of_float (float_of_int n /. 1_000_000_000_000_000.0) ^ " EB"

let binary (n : int) =
  if n < 1000 then string_of_int n ^ " B"
  else
    let exponent = int_of_float (log (float_of_int n) /. log 2.0 /. 10.0) in
    let exponent_char =
      String.get si_prefixes (min exponent (String.length si_prefixes - 1))
    in
    Printf.sprintf "%0.2f %ciB"
      (float_of_int n /. (2.0 ** (float_of_int exponent *. 10.0)))
      exponent_char

let decimal (n : int) =
  if n < 1000 then string_of_int n ^ " B"
  else
    let exponent = int_of_float (log10 (float_of_int n) /. 3.0) in
    let exponent_char =
      String.get si_prefixes (min exponent (String.length si_prefixes - 1))
    in
    Printf.sprintf "%0.2f %cB"
      (float_of_int n /. (10.0 ** (float_of_int exponent *. 3.0)))
      exponent_char
