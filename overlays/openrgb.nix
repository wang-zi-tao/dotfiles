final: prev: with final; {
  openrgb = prev.openrgb.overrideAttrs (oldAttrs: rec {
    version = "a6b890";

    src = final.fetchFromGitLab {
      owner = "CalcProgrammer1";
      repo = "OpenRGB";
      rev = "a6b890a48d75325d47587131c74764b8e9a06a53";
      hash = "sha256-54rkkwgxLOseRJru3RkX2aYE6y4ryzFUCHR423+Ni/I=";
    };

    postPatch = ''
      patchShebangs scripts/build-udev-rules.sh
      substituteInPlace scripts/build-udev-rules.sh \
        --replace-fail "/usr/bin/env chmod" "${coreutils}/bin/chmod"
    '';

    nativeBuildInputs =
      with final;
      oldAttrs.nativeBuildInputs
      ++ [
        bash
      ];

    patches = [ ];
  });
}
