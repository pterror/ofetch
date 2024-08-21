open Tui.Style

let transgender_flag =
  [
    Rgb (rgb 91 206 250);
    Rgb (rgb 245 169 184);
    Rgb (rgb 255 255 255);
    Rgb (rgb 91 206 250);
    Rgb (rgb 245 169 184);
  ]

let nonbinary_flag =
  [
    Rgb (rgb 252 244 52);
    Rgb (rgb 255 255 255);
    Rgb (rgb 156 89 209);
    Rgb (rgb 44 44 44);
  ]

let rainbow_flag =
  [
    Rgb (rgb 230 0 0);
    Rgb (rgb 255 142 0);
    Rgb (rgb 255 239 0);
    Rgb (rgb 0 130 27);
    Rgb (rgb 0 75 255);
    Rgb (rgb 120 0 137);
  ]

let flag_by_name name =
  match name with
  | "transgender" | "trans" | "t" -> transgender_flag
  | "non-binary" | "nonbinary" | "enby" | "nb" -> nonbinary_flag
  | "rainbow" | "r" -> rainbow_flag
  | _ -> transgender_flag
