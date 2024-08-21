type t = {
  codename: string;
  description: string;
  id: string;
  release: string;
  version: string;
}

let lsb_release () =
  let initial_lsb_release =
    {
      codename = "";
      description = "";
      id = "";
      release = "";
      version = "";
    }
  and process_lsb_release line lsb_release =
    let parts = String.split_on_char '=' line in
    match parts with
    | [ name; value_raw ] -> (
        let value =
          if String.get value_raw 0 == '"' then
            String.sub value_raw 1 (String.length value_raw - 2)
          else value_raw
        in
        match name with
        | "DISTRIB_CODENAME" -> { lsb_release with codename = value }
        | "DISTRIB_DESCRIPTION" -> { lsb_release with description = value }
        | "DISTRIB_ID" -> { lsb_release with id = value }
        | "DISTRIB_RELEASE" -> { lsb_release with release = value }
        | "LSB_VERSION" -> { lsb_release with version = value }
        | _ -> lsb_release)
    | _ -> lsb_release
  in
  Io.file_fold_left_lines "/etc/lsb-release" process_lsb_release
    initial_lsb_release
