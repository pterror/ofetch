let csi = "\x1b["
let x10_on = csi ^ "?9h"
let x10_off = csi ^ "?9l"
let button_event_on = csi ^ "?1002h"
let button_event_off = csi ^ "?1002l"
let any_event_on = csi ^ "?1003h"
let any_event_off = csi ^ "?1003l"
let ext_on = csi ^ "?1005h"
let ext_off = csi ^ "?1005l"
let sgr_on = csi ^ "?1006h"
let sgr_off = csi ^ "?1006l"
let urxvt_on = csi ^ "?1015h"
let urxvt_off = csi ^ "?1015l"
let pixel_position_on = csi ^ "?1016h"
let pixel_position_off = csi ^ "?1016l"

type mouse_event = { button : int; x : int; y : int; is_press : bool }

let read_sgr_event s =
  let _ =
    Str.string_match (Str.regexp "\x1b([0-9]+);([0-9]+);([0-9]+)([mM])") s 0
  in
  let button = int_of_string (Str.matched_group 1 s)
  and x = int_of_string (Str.matched_group 2 s)
  and y = int_of_string (Str.matched_group 3 s)
  and is_press = Str.matched_group 3 s == "M" in
  { button; x; y; is_press }
