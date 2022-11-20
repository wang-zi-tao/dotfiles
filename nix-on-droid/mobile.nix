inputs@{ pkgs, lib, config, ... }: {
  imports = [ ../module/nix.nix ];
  environment.packages = with pkgs;[
    gnugrep
    gnused
    command-not-found
  ];
  environment.etcBackupExtension = ".bak";
  home-manager.config = { ... }: (import ../home-manager/profiles/wangzi-mini.nix (inputs // { inherit pkgs; })).configuration;
  home-manager.useGlobalPkgs = true;
  user.shell = "${pkgs.zsh}/bin/zsh";
  system.stateVersion = "22.05";
  terminal.font = "${pkgs.nerdfonts}/share/fonts/truetype/NerdFonts/Iosevka Nerd Font Complete Mono.ttf";
  time.timeZone = "Asia/Shanghai";
}
