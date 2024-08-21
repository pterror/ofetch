type alignment_horizontal = Left | Center | Right
type alignment_vertical = Top | Middle | Bottom
type rgb = { red : int; green : int; blue : int }
type color = Rgb of rgb | StripesHorizontal of color list

type style = {
  fg : color option;
  bg : color option;
  width : int option;
  height : int option;
  align_h : alignment_horizontal option;
  align_v : alignment_vertical option;
  has_padding : bool option;
}

let style ?fg ?bg ?width ?height ?align_h ?align_v ?has_padding () =
  { fg; bg; width; height; align_h; align_v; has_padding }

let rgb r g b = { red = r; green = g; blue = b }
