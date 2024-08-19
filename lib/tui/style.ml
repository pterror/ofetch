type rgb = { red : int; green : int; blue : int }
type color = Rgb of rgb
type style = { fg : color option; bg : color option }

let style ?fg ?bg () = { fg; bg }
let rgb r g b = { red = r; green = g; blue = b }
