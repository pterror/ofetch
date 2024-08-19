type t = {
  ansi_color : string;
  bug_report_url : string;
  build_id : string;
  documentation_url : string;
  home_url : string;
  id : string;
  image_id : string;
  image_version : string;
  logo : string;
  name : string;
  pretty_name : string;
  support_url : string;
  version : string;
  version_codename : string;
  version_id : string;
}

let os_release () =
  let initial_os_release =
    {
      ansi_color = "";
      bug_report_url = "";
      build_id = "";
      documentation_url = "";
      home_url = "";
      id = "";
      image_id = "";
      image_version = "";
      logo = "";
      name = "";
      pretty_name = "";
      support_url = "";
      version = "";
      version_codename = "";
      version_id = "";
    }
  and process_os_release line os_release =
    let parts = String.split_on_char '=' line in
    match parts with
    | [ name; value_raw ] -> (
        let value =
          if String.get value_raw 0 == '"' then
            String.sub value_raw 1 (String.length value_raw - 2)
          else value_raw
        in
        match name with
        | "ANSI_COLOR" -> { os_release with ansi_color = value }
        | "BUG_REPORT_URL" -> { os_release with bug_report_url = value }
        | "BUILD_ID" -> { os_release with build_id = value }
        | "DOCUMENTATION_URL" -> { os_release with documentation_url = value }
        | "HOME_URL" -> { os_release with home_url = value }
        | "ID" -> { os_release with id = value }
        | "IMAGE_ID" -> { os_release with image_id = value }
        | "IMAGE_VERSION" -> { os_release with image_version = value }
        | "LOGO" -> { os_release with logo = value }
        | "NAME" -> { os_release with name = value }
        | "PRETTY_NAME" -> { os_release with pretty_name = value }
        | "SUPPORT_URL" -> { os_release with support_url = value }
        | "VERSION" -> { os_release with version = value }
        | "VERSION_CODENAME" -> { os_release with version_codename = value }
        | "VERSION_ID" -> { os_release with version_id = value }
        | _ -> os_release)
    | _ -> os_release
  in
  Io.file_fold_left_lines "/etc/os-release" process_os_release
    initial_os_release
