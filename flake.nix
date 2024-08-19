{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
  };
  outputs = { self, nixpkgs }:
    let
      forEachSystem = fn: nixpkgs.lib.genAttrs
        nixpkgs.lib.systems.flakeExposed
        (system: fn system nixpkgs.legacyPackages.${system});
    in
    {
      packages = forEachSystem 
        (system: pkgs: rec {
          ofetch = pkgs.callPackage ./default.nix {};
          default = ofetch;
        });
      devShells = forEachSystem
        (system: pkgs: {
          default = import ./shell.nix {
            inherit pkgs;
            inherit (self.packages.${system}) default;
          };
	});
    };
}
