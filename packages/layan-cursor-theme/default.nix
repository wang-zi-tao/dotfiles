{ stdenvNoCC, lib }:

stdenvNoCC.mkDerivation {
  pname = "layan-cursor-theme";
  version = "2021-08-01";

  src = fetchTarball {
    url = "https://github.com/vinceliuice/Layan-cursors/archive/refs/tags/2021-08-01.tar.gz";
    sha256 = "sha256:1bmjcjbn6aff2nv19jz7phlwjgj5w5lghjw79kr4lcrff91kjdr3";
  };

  installPhase = ''
    mkdir -p $out/share/icons
    for theme in "" "-border" "-white" ; do
      cp -r "dist$theme" "$out/share/icons/Layan$theme Cursors"
    done
  '';

  meta = with lib; {
    description = "This is an x-cursor theme inspired by layan gtk theme and based on capitaine-cursors.";
    homepage = "https://github.com/vinceliuice/Layan-cursors";
    platforms = platforms.unix;
    license = with licenses; [ gpl3 ];
    # maintainers = with maintainers; [ frogamic ];
  };
}
