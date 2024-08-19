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

val sysinfo : unit -> t
