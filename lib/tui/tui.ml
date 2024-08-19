module Element = Element
module Style = Style
open Element

exception UnknownElement of unit

let rec render element =
  match element with
  | Text { text; style } -> (
      match style with
      | None -> text
      | Some { fg; bg } ->
          (match fg with
          | None -> ""
          | Some (Rgb { red; green; blue }) -> Ansi.Style.fg_rgb red green blue)
          ^ (match bg with
            | None -> ""
            | Some (Rgb { red; green; blue }) ->
                Ansi.Style.bg_rgb red green blue)
          ^ text
          ^ Ansi.Style.reset)
  | Row { items; _ } -> String.concat " " (List.map render items)
  | Column { items; _ } -> String.concat "\n" (List.map render items)
  | Table { rows; _ } ->
      String.concat "\n" (List.map (function x -> render (TableRow x)) rows)
  | TableRow { items; _ } -> String.concat " " (List.map render items)
  | _ -> raise (UnknownElement ())
