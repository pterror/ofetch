open Ofetch
open Tui.Style
open Tui.Element

let uname = Linux.Uname.uname ()
let sysinfo = Linux.Sysinfo.sysinfo ()
let meminfo = Linux.Proc.Meminfo.read_meminfo ()
let os_release = Linux.Etc.Os_release.os_release ()
let bright_blue = Rgb (rgb 91 206 250)

let info heading info =
  table_row
    [
      text ~style:(style ~fg:bright_blue ~align_h:Left ()) [ heading ^ ":" ];
      text [ info ];
    ]

let () =
  let uptime = Time.seconds_to_string sysinfo.uptime
  and memory_free =
    Byte_sizes.binary (meminfo.memory_total - meminfo.memory_available)
  and memory_total = Byte_sizes.binary meminfo.memory_total
  and swap_free =
    Byte_sizes.binary
      (meminfo.swap_total - meminfo.swap_free - meminfo.swap_cached)
  and swap_total = Byte_sizes.binary meminfo.swap_total
  and username = Sys.getenv "USER"
  and hostname = uname.node_name
  and blue_text = style ~fg:bright_blue ~align_h:Left () in
  table ~style:(style ~gap_h:1 ())
    ([
       info "OS" os_release.pretty_name;
       info "Arch" uname.machine;
       info "Kernel" uname.release;
       info "Uptime" uptime;
       info "Memory" (Printf.sprintf "%s / %s" memory_free memory_total);
     ]
    @ (if meminfo.swap_total > 0 then
         [ info "Swap" (Printf.sprintf "%s / %s" swap_free swap_total) ]
       else [])
    @ [ info "Processes" (string_of_int sysinfo.processes) ])
  |> (fun x ->
       column
         [
           row
             [
               text ~style:blue_text [ username ];
               text [ "@" ];
               text ~style:blue_text [ hostname ];
             ];
           text
             [
               String.make
                 (String.length username + 1 + String.length hostname)
                 '-';
             ];
           x;
         ])
  |> (fun x ->
       row ~style:(style ~gap_h:1 ())
         [
           text
             ~style:
               (style
                  ~fg:(StripesHorizontal (Data.Flags.flag_by_name "trans"))
                  ())
             (String.split_on_char '\n' (Data.Logos.logo_by_name os_release.id));
           x;
         ])
  |> Tui.render
  |> List.iter print_endline
