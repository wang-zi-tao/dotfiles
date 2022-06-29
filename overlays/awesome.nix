final: prev: {
  awesome = (prev.awesome.override {
    lua = final.luajit;
  }).overrideAttrs
    (oldAttrs: {
      version = "2022.5.30";
      src = final.fetchFromGitHub {
        owner = "awesomewm";
        repo = "awesome";
        rev = "3a542219f3bf129546ae79eb20e384ea28fa9798";
        sha256 = "sha256-4z3w6iuv+Gw2xRvhv2AX4suO6dl82woJn0p1nkEx3uM=";
      };
      GI_TYPELIB_PATH = "${oldAttrs.GI_TYPELIB_PATH}:${final.gtk3-x11}/lib/girepository-1.0:${final.atk}/lib/girepository-1.0:${final.playerctl}/lib/girepository-1.0";
    });
}
