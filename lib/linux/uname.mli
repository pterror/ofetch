type t = {
  system_name : string;
  node_name : string;
  release : string;
  version : string;
  machine : string;
  domain_name : string;
}

val uname : unit -> t
