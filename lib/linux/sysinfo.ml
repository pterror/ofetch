open Ctypes
open Foreign

type t = {
  uptime : int;
  load_average_1_minute : int;
  load_average_5_minutes : int;
  load_average_15_minutes : int;
  memory_total : int;
  memory_free : int;
  memory_shared : int;
  memory_buffer : int;
  swap_total : int;
  swap_free : int;
  processes : int;
  high_total : int;
  high_free : int;
  memory_unit : int;
}

type sysinfo_t

let sysinfo_t : sysinfo_t structure typ = structure "sysinfo"
let uptime = field sysinfo_t "uptime" long
let load_1 = field sysinfo_t "load_1" ulong
let load_5 = field sysinfo_t "load_5" ulong
let load_15 = field sysinfo_t "load_15" ulong
let totalram = field sysinfo_t "totalram" ulong
let freeram = field sysinfo_t "freeram" ulong
let sharedram = field sysinfo_t "sharedram" ulong
let bufferram = field sysinfo_t "bufferram" ulong
let totalswap = field sysinfo_t "totalswap" ulong
let freeswap = field sysinfo_t "freeswap" ulong
let procs = field sysinfo_t "procs" ushort
let totalhigh = field sysinfo_t "totalhigh" ulong
let freehigh = field sysinfo_t "freehigh" ulong
let mem_unit = field sysinfo_t "mem_unit" uint
let () = seal sysinfo_t
let times n x = n * x

let from_c c =
  let memory_unit = getf c mem_unit |> Unsigned.UInt.to_int in
  let memory = times memory_unit in
  {
    uptime = getf c uptime |> Signed.Long.to_int;
    load_average_1_minute = getf c load_1 |> Unsigned.ULong.to_int;
    load_average_5_minutes = getf c load_5 |> Unsigned.ULong.to_int;
    load_average_15_minutes = getf c load_15 |> Unsigned.ULong.to_int;
    memory_total = getf c totalram |> Unsigned.ULong.to_int |> memory;
    memory_free = getf c freeram |> Unsigned.ULong.to_int |> memory;
    memory_shared = getf c sharedram |> Unsigned.ULong.to_int |> memory;
    memory_buffer = getf c bufferram |> Unsigned.ULong.to_int |> memory;
    swap_total = getf c totalswap |> Unsigned.ULong.to_int |> memory;
    swap_free = getf c freeswap |> Unsigned.ULong.to_int |> memory;
    processes = getf c procs |> Unsigned.UShort.to_int;
    high_total = getf c totalhigh |> Unsigned.ULong.to_int |> memory;
    high_free = getf c freehigh |> Unsigned.ULong.to_int |> memory;
    memory_unit;
  }

let sysinfo' = foreign "sysinfo" (ptr sysinfo_t @-> returning int)

let sysinfo () =
  let c = make sysinfo_t in
  let _ = sysinfo' (addr c) in
  from_c c
