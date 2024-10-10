pkgs: prev: rec {
  tracy = prev.tracy.overrideAttrs (
    oldAttrs:
    let
      withWayland = false;
    in
    with pkgs;
    rec {
      version = "0.11.1";
      src = pkgs.fetchFromGitHub {
        owner = "wolfpld";
        repo = "tracy";
        rev = "v${version}";
        hash = "sha256-HofqYJT1srDJ6Y1f18h7xtAbI/Gvvz0t9f0wBNnOZK8=";
      };

      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
        pkgs.cmake
        pkgs.meson
      ];

      buildInputs = oldAttrs.buildInputs ++ [
        pkgs.egl-wayland.dev
        pkgs.wayland-scanner.dev
        pkgs.libxkbcommon.dev
        pkgs.kdePackages.wayland-protocols
      ];

      buildPhase = ''
        export CMAKE_INSTALL_INCLUDEDIR=$out/include
        export CMAKE_INSTALL_BINDIR=$out/bin
        export FLAGS="-DDOWNLOAD_CAPSTONE=NO"

        runHook preBuild
        cmake --build .

        mkdir  capture
        cmake -B capture -S $src/capture $FLAGS
        make -j $NIX_BUILD_CORES -C capture

        mkdir  csvexport
        cmake -B csvexport -S $src/csvexport $FLAGS
        make -j $NIX_BUILD_CORES -C csvexport

        mkdir  import
        cmake -B import -S $src/import $FLAGS
        make -j $NIX_BUILD_CORES -C import

        mkdir  profiler
        cmake -B profiler -S $src/profiler $FLAGS \
            ${lib.optionalString (stdenv.hostPlatform.isLinux && !withWayland) "-DLEGACY=1"}
        make -j $NIX_BUILD_CORES -C profiler

        runHook postBuild
      '';

      installPhase = ''
          export CMAKE_INSTALL_INCLUDEDIR=$out/include
          export CMAKE_INSTALL_BINDIR=$out/bin
          mkdir $out/
          runHook preInstall

          install -D -m 0755 capture/tracy-capture $out/bin/tracy-capture
          install -D -m 0755 csvexport/tracy-csvexport $out/bin/tracy-csvexport
          install -D -m 0755 import/tracy-import-chrome $out/bin/tracy-import-chrome
          install -D -m 0755 profiler/tracy-profiler $out/bin/tracy-profiler

          mkdir -p $out/include/Tracy/client
          mkdir -p $out/include/Tracy/common
          mkdir -p $out/include/Tracy/tracy

          cp -p $src/public/client/*.{h,hpp} $out/include/Tracy/client
          cp -p $src/public/common/*.{h,hpp} $out/include/Tracy/common
          cp -p $src/public/tracy/*.{h,hpp} $out/include/Tracy/tracy
        ''
        + lib.optionalString stdenv.hostPlatform.isLinux ''

          install -D -m 0644 $src/extra/desktop/application-tracy.xml $out/share/mime/packages/application-tracy.xml
          install -D -m 0644 $src/extra/desktop/tracy.desktop $out/share/applications/tracy.desktop
          substituteInPlace $out/share/applications/tracy.desktop \
          --replace Exec=/usr/bin/tracy Exec=tracy
          install -D -m 0644 $src/icon/application-tracy.svg $out/share/icons/hicolor/scalable/apps/application-tracy.svg
          install -D -m 0644 $src/icon/icon.png $out/share/icons/hicolor/256x256/apps/tracy.png
          install -D -m 0644 $src/icon/icon.svg $out/share/icons/hicolor/scalable/apps/tracy.svg
        ''
        + ''
          runHook postInstall
        '';
    }
  );
}
