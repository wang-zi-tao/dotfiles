final: prev: rec {
  picom = prev.picom.overrideAttrs (oldAttrs: {
    pname = "picom-animations";
    src = final.pkgs.fetchFromGitHub {
      owner = "jonaburg";
      repo = "picom";
      rev = "a8445684fe18946604848efb73ace9457b29bf80";
      sha256 = "sha256-R+YUGBrLst6CpUgG9VCwaZ+LiBSDWTp0TLt1Ou4xmpQ=";
    };
    nativeBuildInputs = with final; [ makeWrapper ] ++ oldAttrs.nativeBuildInputs;
    buildInputs =
      with final;
      [
        pcre
        libev
      ]
      ++ oldAttrs.buildInputs;
  });
  # picom = prev.nur.repos.xeals.picom-animations;
}
