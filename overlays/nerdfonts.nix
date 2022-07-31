pkgs: prev: {
  nerdfonts = prev.nerdfonts.override {
    enableWindowsFonts = true;
    fonts = [
      "Iosevka"
      "SourceCodePro"
    ];
  };
}
