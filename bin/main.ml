open Ofetch
open Tui.Style
open Tui.Element

let uname = Linux.Uname.uname ()
let sysinfo = Linux.Sysinfo.sysinfo ()
let meminfo = Linux.Proc.Meminfo.read_meminfo ()
let os_release = Linux.Etc.Os_release.os_release ()

let print_section header contents =
  print_string
    (Ansi.Style.fg_bright_blue
    ^ header
    ^ ":"
    ^ Ansi.Style.fg_default
    ^ " "
    ^ contents
    ^ "\n")

let element =
  table
    [
      table_row
        [
          text ~style:(style ~fg:(Rgb (rgb 196 255 255)) ()) "testing!";
          text ~style:(style ~fg:(Rgb (rgb 255 196 255)) ()) "testing! 2";
        ];
      table_row
        [
          text ~style:(style ~fg:(Rgb (rgb 196 255 255)) ()) "t3";
          text ~style:(style ~fg:(Rgb (rgb 255 196 255)) ()) "t4";
        ];
    ]

let () =
  print_endline (Tui.render element);
  print_section "OS" os_release.pretty_name;
  print_section "Arch" uname.machine;
  print_section "Kernel" uname.release;
  print_section "Uptime" (Time.seconds_to_string sysinfo.uptime);
  print_section "Memory"
    (Printf.sprintf "%s / %s"
       (Byte_sizes.binary (meminfo.memory_total - meminfo.memory_available))
       (Byte_sizes.binary meminfo.memory_total));
  if meminfo.swap_total > 0 then
    print_section "Swap"
      (Printf.sprintf "%s / %s"
         (Byte_sizes.binary (meminfo.swap_total - meminfo.swap_free))
         (Byte_sizes.binary meminfo.swap_total));
  print_section "Processes" (string_of_int sysinfo.processes)
