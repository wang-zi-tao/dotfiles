{ pkgs, ... }@inputs:
let hostname = "lxd"; in
{
  modules = [
    # ../module/cluster.nix
  ];
  boot.isContainer = true;
  environment.noXlibs = true;
  systemd.suppressedSystemUnits = [
    "console-getty.service"
    "getty@.service"
    "systemd-udev-trigger.service"
    "systemd-udevd.service"
    "sys-fs-fuse-connections.mount"
    "sys-kernel-debug.mount"
    "dev-mqueue.mount"
  ];
  environment.systemPackages = with pkgs; [
    # xpra
    # virtualgl
    # firefox
    # google-chrome-stable
    # vscode
    nix-tree
    # qt5ct
    # glxinfo
  ];
  networking = {
    interfaces.eth0.useDHCP = true;
    hostName = hostname;
  };
  nixpkgs.config.allowUnfree = true;
  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  # i18n.defaultLocale = "zh_CN.UTF-8";
  # gtk = { iconCache.enable = true; };
  # fonts = {
  #   fontconfig = {
  #     enable = true;
  #     defaultFonts.emoji = [ "Noto Color Emoji" ];
  #     defaultFonts.monospace = [ "Iosevka Custom Medium" "Nerd Mono" ];
  #     defaultFonts.sansSerif =
  #       [ "Iosevka Custom Medium" "Nerd Mono" "Inter" "Liberation Sans" "Soruce Han Sans SC" ];
  #     defaultFonts.serif = [ "Iosevka Custom Medium" "Nerd Mono" "Liberation Serif" "Source Han Serif SC" ];
  #   };
  #   fonts = with pkgs; [
  #     source-code-pro
  #     source-sans-pro
  #     source-han-sans
  #     source-han-sans-simplified-chinese
  #     powerline-fonts
  #     powerline-symbols
  #     fira-code-symbols
  #   ];
  #   fontDir.enable = true;
  # };
  # sound.enable = true;
  # i18n.inputMethod = {
  #   enabled = "ibus";
  #   ibus = { engines = with pkgs.ibus-engines; [ libpinyin ]; };
  #   uim.toolbar = "qt4";
  # };
  # services.xserver = {
  #   displayManager.xpra = {
  #     enable = true;
  #     auth = "password:value=test";
  #     pulseaudio = true;
  #   };
  # };
  # hardware = {
  #   opengl.enable = true;
  #   opengl.setLdLibraryPath = true;
  #   opengl.driSupport = true;
  # };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.root = { ... }: {
    imports = [ ../home-manager/terminal/terminal.nix ];
  };
}
