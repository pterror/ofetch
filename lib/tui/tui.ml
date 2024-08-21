module Element = Element
module Style = Style
open Element
open Std

exception UnknownElement of unit

type size = { width : int; height : int }

let apply_style_size style { width; height } =
  {
    width =
      (fun (s : Style.style) -> s.width)
      |> Option.bind style
      |> Option.value ~default:width;
    height =
      (fun (s : Style.style) -> s.height)
      |> Option.bind style
      |> Option.value ~default:height;
  }

let try_tl list = match list with _ :: tl -> tl | _ -> []

let rec calculate_size element =
  match element with
  | Text { lines; style } ->
      {
        width =
          List.fold_left
            (fun width line -> max (String.length line) width)
            0 lines;
        height = List.length lines;
      }
      |> apply_style_size style
  | Row { items; style } ->
      let gap = 1 in
      let { width; height } =
        List.fold_left
          (fun { width; height } item ->
            let item_size = calculate_size item in
            {
              width = max width item_size.width;
              height = height + item_size.height + gap;
            })
          { width = -gap; height = 0 }
          items
      in
      { width = max 0 width; height } |> apply_style_size style
  | Column { items; style } ->
      let gap = 0 in
      let { width; height } =
        List.fold_left
          (fun { width; height } item ->
            let item_size = calculate_size item in
            {
              width = max width item_size.width;
              height = height + item_size.height + gap;
            })
          { width = 0; height = -gap }
          items
      in
      { width; height = max 0 height } |> apply_style_size style
  | Table { rows; style } ->
      let gap = 1 in
      let width =
        List.fold_left
          (fun w new_w -> w + new_w + gap)
          (-1)
          (table_column_widths (Table { rows; style }))
      and height = List.length rows in
      { width = max width 0; height } |> apply_style_size style
  | _ -> raise (UnknownElement ())

and table_column_widths element =
  match element with
  | Table { rows; style } ->
      let width, done_ =
        List.fold_left
          (fun (width, done_) { items; style } ->
            match items with
            | item :: _ ->
                let item_size = calculate_size item |> apply_style_size style in
                (max width item_size.width, false)
            | _ -> (width, done_))
          (0, true) rows
      in
      if done_ then []
      else
        width
        :: table_column_widths
             (Table
                {
                  rows =
                    List.map
                      (fun { items; style } -> { items = try_tl items; style })
                      rows;
                  style;
                })
  | _ -> []

let rec map2_defaults f a b da db =
  match (a, b) with
  | [], [] -> []
  | _ ->
      let ahd, atl = match a with hd :: tl -> (hd, tl) | _ -> (da, [])
      and bhd, btl = match b with hd :: tl -> (hd, tl) | _ -> (db, []) in
      f ahd bhd :: map2_defaults f atl btl da db

let default_element = text []

let get_style element =
  match element with
  | Text { style; _ } -> style
  | Row { style; _ } -> style
  | Column { style; _ } -> style
  | Table { style; _ } -> style
  | _ -> None

let rec to_rgb index count color =
  Option.bind color (fun color ->
      match color with
      | Style.Rgb { red; green; blue } ->
          let color : Style.rgb = { red; green; blue } in
          Some color
      | Style.StripesHorizontal stripes ->
          let length = List.length stripes in
          length * index / count
          |> min (length - 1)
          |> List.nth_opt stripes
          |> to_rgb index count)

let fg_rgb index count fg =
  match to_rgb index count fg with
  | None -> ""
  | Some { red; green; blue } -> Ansi.Style.fg_rgb red green blue

let bg_rgb index count fg =
  match to_rgb index count fg with
  | None -> ""
  | Some { red; green; blue } -> Ansi.Style.bg_rgb red green blue

let rec render element =
  match element with
  | Text { lines; style } -> (
      let has_padding =
        (fun s -> s.has_padding)
        |> Option.bind style
        |> Option.value ~default:true
      in
      let width = (calculate_size element).width in
      let lines_rect =
        List.map
          (fun line ->
            line
            ^
            if has_padding then String.make (width - String.length line) ' '
            else "")
          lines
      in
      match style with
      | None -> lines_rect
      | Some { fg; bg; _ } ->
          let count = List.length lines in
          List.mapi
            (fun index line ->
              fg_rgb index count fg
              ^ bg_rgb index count bg
              ^ line
              ^ Ansi.Style.reset)
            lines_rect)
  | Row { items; _ } ->
      concat_horizontally (List.map render items)
        (List.map (fun item -> (calculate_size item).width) items)
  | Column { items; _ } ->
      let width = (calculate_size element).width in
      List.flatten
        (List.map
           (fun item ->
             let item_width = (calculate_size item).width in
             let padding = String.make (width - item_width) ' '
             and item_lines = render item in
             if padding == "" then item_lines
             else List.map (fun line -> line ^ padding) item_lines)
           items)
  | Table { rows; style } ->
      let column_widths = table_column_widths (Table { rows; style }) in
      List.flatten
        (List.map
           (fun { items; _ } ->
             concat_horizontally
               (map2_defaults
                  (fun item width ->
                    let padding_width =
                      max 0 (width - (calculate_size item).width)
                    and rendered = render item
                    and horizontal_align =
                      Option.value ~default:Style.Left
                        (Option.bind (get_style item) (fun s -> s.align_h))
                    in
                    match horizontal_align with
                    | Left ->
                        let padding = String.make padding_width ' ' in
                        List.map (flip ( ^ ) padding) rendered
                    | Center ->
                        let padding_left = String.make (padding_width / 2) ' '
                        and padding_right =
                          String.make ((padding_width + 1) / 2) ' '
                        in
                        List.map
                          (fun line -> padding_left ^ line ^ padding_right)
                          rendered
                    | Right ->
                        let padding = String.make padding_width ' ' in
                        List.map (( ^ ) padding) rendered)
                  items column_widths default_element 0)
               column_widths)
           rows)
  | _ -> raise (UnknownElement ())

and concat_horizontally rendered widths =
  let line, done_, _ =
    List.fold_left2
      (fun (acc, done_, first) lines width ->
        let prefix = if first then "" else " " in
        match lines with
        | line :: _ -> (acc ^ prefix ^ line, false, false)
        | _ -> (acc ^ prefix ^ String.make width ' ', done_, false))
      ("", true, true) rendered widths
  in
  if done_ then []
  else line :: concat_horizontally (List.map try_tl rendered) widths
