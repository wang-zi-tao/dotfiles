final: prev: rec {
  iosevka = prev.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-terminal]
      family = "Iosevka Terminal"
      spacing = "normal"
      serifs = "sans"
      no-cv-ss = true

        [buildPlans.iosevka-terminal.ligations]
        inherits = "dlig"

      [buildPlans.iosevka-terminal.weights.regular]
      shape = 400
      menu = 400
      css = 400

      [buildPlans.iosevka-terminal.weights.medium]
      shape = 500
      menu = 500
      css = 500

      [buildPlans.iosevka-terminal.weights.semibold]
      shape = 600
      menu = 600
      css = 600

      [buildPlans.iosevka-terminal.widths.normal]
      shape = 600
      menu = 5
      css = "normal"
    '';
    set = "term";
  };

}
