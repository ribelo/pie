{
  description = "Camelpie OCaml development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
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
      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          oxCamlSwitch = "5.2.0+ox";
          sharedOpamRoot = "$HOME/.cache/opam";

          oxCamlSetup = pkgs.writeShellApplication {
            name = "camelpie-oxcaml-init";
            runtimeInputs = [
              pkgs.git
              pkgs.opam
              pkgs.rsync
            ];
            text = ''
              switch_name="''${1:-${oxCamlSwitch}}"
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMYES=1

              if [ ! -d "$OPAMROOT" ]; then
                opam init --bare --disable-sandboxing --no-setup --yes
              fi

              if ! opam switch list --short | grep -Fxq "$switch_name"; then
                opam switch create "$switch_name" \
                  --repos ox=git+https://github.com/oxcaml/opam-repository.git,default \
                  --assume-depexts \
                  --yes
              fi

              export OPAMSWITCH="$switch_name"
              eval "$(opam env --switch "$switch_name" --set-switch)"

              eta_dir="$repo_root/.reference/Eta"
              eta_pin_dir="$repo_root/.opam-eta-src"
              if [ -d "$eta_dir" ]; then
                mkdir -p "$eta_pin_dir"
                rsync -aL --delete \
                  --exclude '.git' \
                  --exclude '_build' \
                  --exclude '_build_*' \
                  --exclude '_opam*' \
                  --exclude '.opam-oxcaml' \
                  --exclude '.hill-climbing' \
                  --exclude '.motel-data' \
                  --exclude '.reference' \
                  --exclude '.scratch' \
                  --exclude 'bench' \
                  --exclude 'http-testsuite/results' \
                  "$eta_dir"/ "$eta_pin_dir"/

                opam pin add --kind=path eta "$eta_pin_dir" --no-action --yes
                for package in \
                  eta_ai \
                  eta_ai_openai_codec \
                  eta_ai_openrouter \
                  eta_blocking \
                  eta_eio \
                  eta_http \
                  eta_http_eio \
                  eta_http_h2 \
                  eta_redacted \
                  eta_stream
                do
                  opam pin add --kind=path "$package" "$eta_pin_dir" --no-action --yes
                done
              fi

              # eio_linux.1.3+ox is not compatible with uring 2.x.
              opam install 'uring=0.9' --assume-depexts --yes
              opam install . --deps-only --with-test --assume-depexts --yes
              opam install \
                'dune=3.22.2+ox' \
                'ocamlformat=0.26.2+ox1' \
                'merlin=5.2.1-502+ox1' \
                'ocaml-lsp-server=1.19.0+ox1' \
                --assume-depexts \
                --yes

              echo "OxCaml switch is ready: $switch_name"
              echo "Shared OPAMROOT: $OPAMROOT"
              echo "Enter it with: nix develop .#oxcaml"
            '';
          };

          oxCamlToolchainCheck = pkgs.writeShellApplication {
            name = "camelpie-oxcaml-check-toolchain";
            runtimeInputs = [
              pkgs.git
              pkgs.opam
            ];
            text = ''
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMSWITCH="${oxCamlSwitch}"
              eval "$(opam env --switch "${oxCamlSwitch}" --set-switch)"

              test "$(ocamlc -version)" = "${oxCamlSwitch}"
              test "$(dune --version)" = "3.22.2"
              opam list --installed --short ocamlformat | grep -Fxq ocamlformat
              opam list --installed --short merlin | grep -Fxq merlin
              opam list --installed --short ocaml-lsp-server | grep -Fxq ocaml-lsp-server
            '';
          };

          oxCamlTest = pkgs.writeShellApplication {
            name = "camelpie-oxcaml-test";
            runtimeInputs = [
              pkgs.git
              pkgs.opam
            ];
            text = ''
              switch_name="''${1:-${oxCamlSwitch}}"
              repo_root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || pwd)"
              cd "$repo_root"

              export OPAMROOT="''${OPAMROOT:-${sharedOpamRoot}}"
              export OPAMSWITCH="$switch_name"
              eval "$(opam env --switch "$switch_name" --set-switch)"

              dune build packages/camelpie
              dune runtest packages/camelpie --force
            '';
          };

          oxCamlHostPackages =
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
              oxCamlSetup
              oxCamlTest
              oxCamlToolchainCheck
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
            if [ -d "$OPAMROOT/${oxCamlSwitch}" ]; then
              export OPAMSWITCH="${oxCamlSwitch}"
              eval "$(opam env --switch "${oxCamlSwitch}" --set-switch)"
            fi
            echo "Camelpie OxCaml shell (${oxCamlSwitch})"
            echo "Shared OPAMROOT: $OPAMROOT"
            echo "Run 'camelpie-oxcaml-init' once to create/update the shared switch."
            echo "Run 'camelpie-oxcaml-test' after setup to build and test shipped packages."
          '';
        in
        {
          default = pkgs.mkShell {
            packages = oxCamlHostPackages;
            inherit shellHook;
          };

          oxcaml = pkgs.mkShell {
            packages = oxCamlHostPackages;
            inherit shellHook;
          };
        }
      );
    };
}
