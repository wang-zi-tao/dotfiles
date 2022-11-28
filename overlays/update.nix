final: prev: {
  fd = prev.rofi.overrideAttrs (oldAttrs: {
    src = final.pkgs.fetchFromGitHub {
      owner = "sharkdp";
      repo = "fd";
      rev = "v8.5.3";
      sha256 = "sha256-dEtIies6AGJqqCdJYu+Aasy25rmoMqrZbG0GtvYNCuc=";
    };
  });
}
