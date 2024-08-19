let csi = "\x1b["
let up n = csi ^ string_of_int n ^ "A"
let down n = csi ^ string_of_int n ^ "B"
let right n = csi ^ string_of_int n ^ "C"
let left n = csi ^ string_of_int n ^ "D"

let move dx dy =
  (if dx == 0 then "" else if dx < 0 then up (-dx) else down dx)
  ^ if dy == 0 then "" else if dy < 0 then left (-dy) else right dy

let up_lines n = csi ^ string_of_int n ^ "E"
let down_lines n = csi ^ string_of_int n ^ "F"
let move_lines n = if n < 0 then up_lines (-n) else down_lines n
let x n = csi ^ string_of_int n ^ "G"
let position x y = csi ^ string_of_int x ^ ";" ^ string_of_int y ^ "H"

let position_format_effector x y =
  csi ^ string_of_int x ^ ";" ^ string_of_int y ^ "f"

let save_position = csi ^ "s"
let restore_position = csi ^ "u"
let show = csi ^ "?25h"
let hide = csi ^ "?25l"
