{
  description = "PIE - Personal Intelligence Engine: Voice-activated AI assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        libs = with pkgs; [
          alsa-lib
          alsa-lib.dev
          pipewire
          pipewire.dev
          pulseaudio
          pulseaudio.dev
          libpulseaudio
          libxkbcommon
          libxkbcommon.dev
          wayland
        ];

        source = builtins.path { path = ./.; name = "pie-source"; };

        piePackage = pkgs.rustPlatform.buildRustPackage {
          pname = "pie";
          version = "0.1.0";
          src = source;
          postPatch = ''rm -rf target'';
          cargoLock = {
            lockFile = ./Cargo.lock;
            outputHashes = {
              "ai-ox-0.1.0" = "sha256-Tb3s4VfhXCmfM9czWw9/M7SHuj+gfRlH9zU78mjetDU=";
            };
          };
          cargoBuildFlags = [ "--features" "audio,wayland" ];
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = libs;
          doCheck = false;
        };

        native-libs = with pkgs; [
          cmake
          pkg-config
          rustc
          cargo
        ];
      in
      {
        packages = {
          pie = piePackage;
          default = piePackage;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = piePackage;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = native-libs;
          buildInputs = libs;
          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath libs}:./"
            export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib.outPath}/lib:$LD_LIBRARY_PATH"
          '';
        };
      });
}
