with import <nixpkgs> { };
let ocamlPackages' = ocamlPackages.overrideScope (self: super: {
  ocaml = super.ocaml.override { flambdaSupport = true; };
});
in mkShell rec {
  nativeBuildInputs = with ocamlPackages'; [
    ocaml
    pkg-config # for ctypes-foreign
    libffi # for ctypes-foreign
    dune_3
    opam
    ocaml-lsp
    ocamlformat
  ];
}
