{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    opam-nix.url = github:tweag/opam-nix;
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let
      t = x: builtins.trace x x;
      name = "ofetch";
      forEachSystem = fn: nixpkgs.lib.genAttrs
        [ "x86_64-linux" ] # nixpkgs.lib.systems.flakeExposed
        (system: fn system nixpkgs.legacyPackages.${system});
      devPackagesQuery = {
        ocaml-lsp-server = "*";
        ocamlformat = "*";
      };
      query = devPackagesQuery // { };
      getPackages = system: pkgs: legacyPackages:
        let
          on = opam-nix.lib.${system};
          scope = legacyPackages.${system};
          localPackagesQuery = builtins.mapAttrs (_: pkgs.lib.last)
            (on.listRepo (on.makeOpamRepo ./.));
        in
        pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope;
    in
    rec {
      legacyPackages = forEachSystem (system: pkgs:
        let
          on = opam-nix.lib.${system};
          scope = on.buildDuneProject { } name ./. query;
          overlay = self: super: {
            ${name} = super.${name}.overrideAttrs (_: {
              doNixSupport = false;
            });
            ctypes = super.ctypes.overrideAttrs (prev: {
              nativeBuildInputs = prev.nativeBuildInputs ++ (with pkgs; [
                pkg-config
                libffi
              ]);
            });
          };
        in
        scope.overrideScope overlay);
      packages = forEachSystem (system: pkgs:
        let
          packages = getPackages system pkgs legacyPackages;
        in
        packages // { default = packages.ofetch; });
      devShells = forEachSystem (system: pkgs:
        let
          scope = legacyPackages.${system};
          packages = getPackages system pkgs legacyPackages;
          devPackages = builtins.attrValues
            (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
        in
        {
          default = pkgs.mkShell {
            inputsFrom = builtins.attrValues packages;
            buildInputs = devPackages ++ (with pkgs; [ ocaml ]);
          };
        });
    };
}
