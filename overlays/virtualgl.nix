final: prev: rec {
  virtualglLib = prev.virtualglLib.overrideAttrs (oldAttrs: rec{
    version = "3.0";
    src = final.fetchurl {
      url = "mirror://sourceforge/virtualgl/VirtualGL-${version}.tar.gz";
      sha256 = "1giin3jmcs6y616bb44bpz30frsmj9f8pz2vg7jvb9vcfc9456rr";
    };
  });
  virtualgl = prev.virtualgl.override { inherit virtualglLib; };
}
