let sgr (s : string) = "\x1b[" ^ s ^ "m"
let reset = sgr "0"
let bold = sgr "1"
let faint = sgr "2"
let italic = sgr "3"
let underline = sgr "4"
let blink_slow = sgr "5"
let blink_fast = sgr "6"
let invert = sgr "7"
let conceal = sgr "8"
let strikethrough = sgr "9"
let font n = sgr (string_of_int (n + 10))
let font_fraktur = sgr "20"
let bold_off = sgr "21"
let bold_faint_off = sgr "22"
let italic_blackletter_off = sgr "23"
let underline_off = sgr "24"
let blink_off = sgr "25"
let proportional_spacing = sgr "26"
let reversed_off = sgr "27"
let conceal_off = sgr "28"
let strikethrough_off = sgr "29"
let fg_black = sgr "30"
let fg_red = sgr "31"
let fg_green = sgr "32"
let fg_yellow = sgr "33"
let fg_blue = sgr "34"
let fg_magenta = sgr "35"
let fg_cyan = sgr "36"
let fg_white = sgr "37"
let fg_light_gray = sgr "37"
let fg_gray = sgr "90"
let fg_bright_black = sgr "90"
let fg_bright_red = sgr "91"
let fg_bright_green = sgr "92"
let fg_bright_yellow = sgr "93"
let fg_bright_blue = sgr "94"
let fg_bright_magenta = sgr "95"
let fg_bright_cyan = sgr "96"
let fg_bright_white = sgr "97"
let fg_256color n = sgr ("38;5;" ^ string_of_int n)

let fg_rgb r g b =
  sgr ("38;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b)

let fg_default = sgr "39"
let bg_black = sgr "40"
let bg_red = sgr "41"
let bg_green = sgr "42"
let bg_yellow = sgr "43"
let bg_blue = sgr "44"
let bg_magenta = sgr "45"
let bg_cyan = sgr "46"
let bg_white = sgr "47"
let bg_light_gray = sgr "47"
let bg_gray = sgr "100"
let bg_bright_black = sgr "100"
let bg_bright_red = sgr "101"
let bg_bright_green = sgr "102"
let bg_bright_yellow = sgr "103"
let bg_bright_blue = sgr "104"
let bg_bright_magenta = sgr "105"
let bg_bright_cyan = sgr "106"
let bg_bright_white = sgr "107"
let bg_256color n = sgr ("48;5;" ^ string_of_int n)

let bg_rgb r g b =
  sgr ("48;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b)

let bg_default = sgr "49"
let framed = sgr "51"
let encircled = sgr "52"
let overlined = sgr "53"
let framed_encircled_off = sgr "54"
let overlined_off = sgr "55"
