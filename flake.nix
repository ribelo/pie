{
  description = "Pie OCaml development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.eta = {
    url = "git+file:///home/ribelo/projects/ribelo/ocaml/Eta";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.opam-nix.follows = "opam-nix";
    inputs.opam-repository.follows = "opam-repository";
    inputs.oxcaml.follows = "oxcaml";
  };
  inputs.opam-nix = {
    url = "github:tweag/opam-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.opam-repository = {
    url = "github:ocaml/opam-repository";
    flake = false;
  };
  inputs.oxcaml = {
    url = "github:oxcaml/oxcaml/5.2.0minus-31";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      eta,
      opam-nix,
      opam-repository,
      oxcaml,
      ...
    }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          cleanLocalSource =
            src:
            pkgs.lib.cleanSourceWith {
              inherit src;
              filter =
                path: type:
                let
                  base = baseNameOf path;
                in
                !(
                  base == ".git"
                  || base == "_build"
                  || base == "_opam"
                  || base == ".opam"
                  || base == ".opam-oxcaml"
                  || base == ".reference"
                  || base == ".scratch"
                  || base == ".motel-data"
                  || base == ".nema"
                  || base == ".pi"
                  || base == ".hill-climbing"
                  || base == "node_modules"
                );
            };
          pieSrc = cleanLocalSource ./.;
          etaSrc = cleanLocalSource eta;
          opamLib = opam-nix.lib.${system};
          etaRepo = opamLib.makeOpamRepo etaSrc;
          patchedOxcaml = pkgs.runCommand "oxcaml-source-patched" { } ''
            cp -R ${oxcaml} "$out"
            chmod -R u+w "$out"
            ${pkgs.perl}/bin/perl -0pi -e 's|new: old: \{\n        buildInputs = \[|new: old: rec {\n        version = "20231231";\n        src = pkgs.fetchFromGitLab {\n          domain = "gitlab.inria.fr";\n          owner = "fpottier";\n          repo = "menhir";\n          rev = version;\n          sha256 = "sha256-veB0ORHp6jdRwCyDDAfc7a7ov8sOeHUmiELdOFf/QYk=";\n        };\n        patches = [ ];\n        buildInputs = [|' "$out/default.nix"
          '';
          oxcamlCompiler = pkgs.callPackage "${patchedOxcaml}/default.nix" {
            src = patchedOxcaml;
            ocamltest = false;
            warnError = false;
          };
          oxcamlSystemOverlay = final: prev: {
            ocaml-system = prev.ocaml-system.overrideAttrs (_old: {
              nativeBuildInputs = [ oxcamlCompiler ];
            });
          };
          scope = opamLib.buildOpamProject
            {
              inherit pkgs;
              overlays = [ oxcamlSystemOverlay ];
              repos = [
                etaRepo
                opam-repository
              ];
              resolveArgs.env.sys-ocaml-version = "5.2.0";
            }
            "pie"
            pieSrc
            {
              ocaml-system = "*";
            };

          pie = scope.pie;
        in
        {
          default = pie;
          inherit pie;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          switchName = "5.2.0+ox";
          sharedOpamRoot = "$HOME/.cache/opam";

          setup = pkgs.writeShellApplication {
            name = "pie-opam-install";
            runtimeInputs = [
              pkgs.autoconf
              pkgs.coreutils
              pkgs.git
              pkgs.gnumake
              pkgs.m4
              pkgs.nix
              pkgs.opam
              pkgs.patch
              pkgs.pkg-config
            ];
            text = ''
              switch_name="''${1:-${switchName}}"
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMYES=1

              if ! opam switch list --short | grep -Fxq "$switch_name"; then
                echo "Missing OPAM switch '$switch_name'." >&2
                echo "Run: shared-ocaml-switch-init" >&2
                exit 1
              fi

              export OPAMSWITCH="$switch_name"
              eval "$(opam env --switch "$switch_name" --set-switch)"

              eta_path="''${ETA_PATH:-$repo_root/../ocaml/Eta}"
              if command -v eta-opam-install-ox >/dev/null 2>&1; then
                ETA_REPO_ROOT="$eta_path" eta-opam-install-ox
              elif [ -d "$eta_path" ]; then
                ETA_REPO_ROOT="$eta_path" nix develop "$eta_path" -c eta-opam-install-ox
              else
                echo "Missing Eta checkout at $eta_path" >&2
                exit 1
              fi

              opam pin add --kind=git pie "git+file://$repo_root#master" --no-action --yes
              opam install pie --assume-depexts --yes

              echo "Pie is installed into OPAM switch: $switch_name"
              echo "Shared OPAMROOT: $OPAMROOT"
              echo "Enter it with: nix develop"
            '';
          };

          toolchainCheck = pkgs.writeShellApplication {
            name = "pie-opam-check-toolchain";
            runtimeInputs = [
              pkgs.git
              pkgs.opam
            ];
            text = ''
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMSWITCH="${switchName}"
              eval "$(opam env --switch "${switchName}" --set-switch)"

              test "$(ocamlc -version)" = "${switchName}"
              dune --version >/dev/null
              opam list --installed --short ocamlformat | grep -Fxq ocamlformat
              opam list --installed --short merlin | grep -Fxq merlin
              opam list --installed --short ocaml-lsp-server | grep -Fxq ocaml-lsp-server
            '';
          };

          test = pkgs.writeShellApplication {
            name = "pie-test";
            runtimeInputs = [
              pkgs.git
              pkgs.opam
            ];
            text = ''
              switch_name="''${1:-${switchName}}"
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMSWITCH="$switch_name"
              eval "$(opam env --switch "$switch_name" --set-switch)"

              dune build packages/pie
              dune runtest packages/pie --force
            '';
          };

          hostPackages =
            [
              pkgs.autoconf
              pkgs.cacert
              pkgs.curl
              pkgs.git
              pkgs.gnumake
              pkgs.m4
              pkgs.opam
              pkgs.patch
              pkgs.pkg-config
              pkgs.unzip
              pkgs.which
              setup
              test
              toolchainCheck
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              pkgs.gmp
              pkgs.libffi
              pkgs.openssl
              pkgs.sqlite
              pkgs.zlib
            ];

          shellHook = ''
            export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
            if [ -d "$OPAMROOT/${switchName}" ]; then
              export OPAMSWITCH="${switchName}"
              eval "$(opam env --switch "${switchName}" --set-switch)"
            fi
            if [ -t 1 ]; then
              echo "Pie OxCaml shell (${switchName})"
              echo "Shared OPAMROOT: $OPAMROOT"
              echo "Run 'shared-ocaml-switch-init' before 'pie-opam-install'."
              echo "Run 'pie-test' after setup to build and test shipped packages."
            fi
          '';
        in
        {
          default = pkgs.mkShell {
            packages = hostPackages;
            inherit shellHook;
          };
        }
      );
    };
}
