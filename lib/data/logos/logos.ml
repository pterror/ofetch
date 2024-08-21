let logo_by_name distro =
  match String.lowercase_ascii distro with
  | "arch" | "arch linux" -> Arch.logo
  | "nix" | "nixos" -> Nixos.logo
  | _ -> Linux_logo.logo
