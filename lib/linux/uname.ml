open Ctypes
open Foreign

type t = {
  system_name : string;
  node_name : string;
  release : string;
  version : string;
  machine : string;
  domain_name : string;
}

type utsname_t

let utsname_t : utsname_t structure typ = structure "utsname"
let sysname = field utsname_t "sysname" (array 65 char)
let nodename = field utsname_t "nodename" (array 65 char)
let release = field utsname_t "release" (array 65 char)
let version = field utsname_t "version" (array 65 char)
let machine = field utsname_t "machine" (array 65 char)
let domainname = field utsname_t "domainname" (array 65 char)
let () = seal utsname_t
let coerce_to_string p = p |> CArray.start |> coerce (ptr char) string

let from_c c =
  {
    system_name = getf c sysname |> coerce_to_string;
    node_name = getf c nodename |> coerce_to_string;
    release = getf c release |> coerce_to_string;
    version = getf c version |> coerce_to_string;
    machine = getf c machine |> coerce_to_string;
    domain_name = getf c domainname |> coerce_to_string;
  }

let uname' = foreign "uname" (ptr utsname_t @-> returning int)

let uname () =
  let c = make utsname_t in
  let _ = uname' (addr c) in
  from_c c
