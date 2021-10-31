final: prev: rec {
  iosevka = prev.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-custom]
      family = "Iosevka Custom"
      spacing = "term"
      serifs = "sans"
      no-cv-ss = true

        [buildPlans.iosevka-custom.ligations]
        inherits = "haskell"

      [buildPlans.iosevka-custom.weights.regular]
      shape = 400
      menu = 400
      css = 400

      [buildPlans.iosevka-custom.weights.medium]
      shape = 500
      menu = 500
      css = 500

      [buildPlans.iosevka-custom.weights.semibold]
      shape = 600
      menu = 600
      css = 600

      [buildPlans.iosevka-custom.weights.bold]
      shape = 700
      menu = 700
      css = 700

      [buildPlans.iosevka-custom.weights.extrabold]
      shape = 800
      menu = 800
      css = 800

      [buildPlans.iosevka-custom.weights.heavy]
      shape = 900
      menu = 900
      css = 900

      [buildPlans.iosevka-custom.widths.normal]
      shape = 600
      menu = 5
      css = "normal"
    '';
    set = "custom";
  };

}
