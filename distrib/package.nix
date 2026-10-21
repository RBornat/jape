{ lib
, stdenv
, ocaml-ng
, openjdk17
, ant
, makeDesktopItem
, copyDesktopItems
}:

let
  ocamlPackages = ocaml-ng.ocamlPackages_4_14;
in
stdenv.mkDerivation (finalAttrs: {
  pname = "jape";
  version = "9.1.8b";

  src = lib.cleanSource ./.;

  nativeBuildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.ocamlbuild
    openjdk17
    ant
    copyDesktopItems
  ];

  env.JAVA_TOOL_OPTIONS = "-Dfile.encoding=UTF-8";
  env.LC_ALL = "C.UTF-8";

  buildPhase = ''
    runHook preBuild

    export JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF-8"
    export ANT_OPTS="-Dfile.encoding=UTF-8"
    export LC_ALL="C.UTF-8"

    # Upstream version.sh relies on git describe, which fails inside the nix sandbox.
    # Provide a simple standalone version.sh script.
    cat << 'EOF' > CommonBuildResources/version.sh
#!/bin/sh
echo "${finalAttrs.version}"
EOF
    chmod +x CommonBuildResources/version.sh

    # Build the engine, GUI, and minimal runtime JRE
    cd CommonBuildResources/Linux
    make app OS=Linux
    cd ../..

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/share/jape" "$out/bin"

    # Copy the assembled Jape application files
    cp -pR CommonBuildResources/Linux/Jape.app/* "$out/share/jape/"
    cp -pR examples "$out/share/jape/examples"

    # Install launcher script
    cat << 'EOF' > "$out/bin/jape"
#!/usr/bin/env bash
set -euo pipefail

SHAREDIR="@out@/share/jape"

args=()
for arg in "$@"; do
  if [ -e "$arg" ]; then
    args+=("$(realpath "$arg")")
  else
    args+=("$arg")
  fi
done

cd "$SHAREDIR"
exec "$SHAREDIR/jre/bin/java" -Duk.org.jape.AppPackage="$SHAREDIR" -m uk.org.jape/uk.org.jape.Jape "''${args[@]}"
EOF
    substituteInPlace "$out/bin/jape" --subst-var out
    chmod +x "$out/bin/jape"

    # Symlink proof engine binary
    ln -s "$out/share/jape/jape_engine" "$out/bin/jape-engine"

    # Install application icons
    for size in 16 32 128 256 512; do
      if [ -f "iconset/icon_''${size}x''${size}.png" ]; then
        mkdir -p "$out/share/icons/hicolor/''${size}x''${size}/apps"
        cp "iconset/icon_''${size}x''${size}.png" "$out/share/icons/hicolor/''${size}x''${size}/apps/jape.png"
      fi
    done
    mkdir -p "$out/share/pixmaps"
    cp java_GUI/Pics/japeicon.png "$out/share/pixmaps/jape.png"

    runHook postInstall
  '';

  desktopItems = [
    (makeDesktopItem {
      name = "jape";
      desktopName = "Jape";
      genericName = "Proof Calculator";
      comment = "A configurable proof calculator for formal inference systems";
      exec = "jape %F";
      icon = "jape";
      terminal = false;
      categories = [ "Education" "Science" "Math" "Development" ];
      startupWMClass = "uk-org-jape-Jape";
    })
  ];

  meta = with lib; {
    description = "A configurable proof calculator for formal inference systems";
    homepage = "https://github.com/RBornat/jape";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
    mainProgram = "jape";
  };
})
