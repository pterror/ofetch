open Ofetch
open Tui.Style
open Tui.Element

let uname = Linux.Uname.uname ()
let sysinfo = Linux.Sysinfo.sysinfo ()
let meminfo = Linux.Proc.Meminfo.read_meminfo ()
let os_release = Linux.Etc.Os_release.os_release ()

let bright_blue = Rgb (rgb 196 255 255)

let info_row heading info =
  table_row
    [
      text ~style:(style ~fg:bright_blue ()) (heading ^ ":");
      text info;
    ]

let () =
  table
    ([
       info_row "OS" os_release.pretty_name;
       info_row "Arch" uname.machine;
       info_row "Kernel" uname.release;
       info_row "Uptime" (Time.seconds_to_string sysinfo.uptime);
       info_row "Memory"
         (Printf.sprintf "%s / %s"
            (Byte_sizes.binary
               (meminfo.memory_total - meminfo.memory_available))
            (Byte_sizes.binary meminfo.memory_total));
     ]
    @ (if meminfo.swap_total > 0 then
         [
           info_row "Swap"
             (Printf.sprintf "%s / %s"
                (Byte_sizes.binary (meminfo.swap_total - meminfo.swap_free))
                (Byte_sizes.binary meminfo.swap_total));
         ]
       else [])
    @ [ info_row "Processes" (string_of_int sysinfo.processes) ])
  |> Tui.render
  |> print_endline
