open Angstrom

(* FIXME: p should be concatted with list_items p *)
let rec list_items p =
  p >>| (fun x -> [ x ]) <* char ',' *> list_items p <|> return []

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go

let list p = char '[' *> list_items p <* char ']'

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
