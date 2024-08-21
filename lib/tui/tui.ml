module Element = Element
module Style = Style
open Element

exception UnknownElement of unit

type size = { width : int; height : int }

let apply_style_size style { width; height } =
  {
    width =
      Option.value ~default:width
        (Option.bind style (fun (s : Style.style) -> s.width));
    height =
      Option.value ~default:height
        (Option.bind style (fun (s : Style.style) -> s.height));
  }

type table_column_sizes_accumulator = { width : int; done_ : bool }

let rec calculate_size element =
  match element with
  | Text { text; style } ->
      { width = String.length text; height = 1 } |> apply_style_size style
  | Row { items; style } -> row_size items style
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
  | TableRow { items; style } -> row_size items style
  | _ -> raise (UnknownElement ())

and row_size items style =
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

and table_column_widths element =
  match element with
  | Table { rows; style } ->
      let { width; done_ } =
        List.fold_left
          (fun { width; done_ } { items; style } ->
            match items with
            | item :: _ ->
                let item_size = calculate_size item |> apply_style_size style in
                { width = max width item_size.width; done_ = false }
            | _ -> { width; done_ })
          { width = 0; done_ = true }
          rows
      in
      if done_ then []
      else
        width
        :: table_column_widths
             (Table
                {
                  rows =
                    List.map
                      (fun { items; style } ->
                        let next_items =
                          match items with _ :: tl -> tl | _ -> []
                        in
                        { items = next_items; style })
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

let default_element = text ""

let get_style element =
  match element with
  | Text { style; _ } -> style
  | Row { style; _ } -> style
  | Column { style; _ } -> style
  | Table { style; _ } -> style
  | TableRow { style; _ } -> style
  | _ -> None

let rec render element =
  match element with
  | Text { text; style } -> (
      match style with
      | None -> text
      | Some { fg; bg; _ } ->
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
  | Table { rows; style } ->
      let column_widths = table_column_widths (Table { rows; style }) in
      String.concat "\n"
        (List.map
           (fun { items; _ } ->
             String.concat " "
               (map2_defaults
                  (fun item width ->
                    let padding = max 0 (width - (calculate_size item).width)
                    and rendered = render item
                    and horizontal_align =
                      Option.value ~default:Style.Left
                        (Option.bind (get_style item) (fun s -> s.align_h))
                    in
                    match horizontal_align with
                    | Left -> rendered ^ String.make padding ' '
                    | Center ->
                        String.make (padding / 2) ' '
                        ^ rendered
                        ^ String.make ((padding + 1) / 2) ' '
                    | Right -> String.make padding ' ' ^ rendered)
                  items column_widths default_element 0))
           rows)
  | TableRow { items; _ } -> String.concat " " (List.map render items)
  | _ -> raise (UnknownElement ())
