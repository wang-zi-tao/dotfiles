{ pkgs, ... }: {
  imports = [ ./picom.nix ./rofi.nix ./polybar.nix ./dunst.nix ./eww/home.nix ];
  home.packages = [ pkgs.lxappearance pkgs.lightlocker pkgs.flashfocus ];
  xsession = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: with hp; [ dbus monad-logger xmonad-contrib xmobar ];
      config = ./xmonad.hs;
    };
  };
  home.file.".xmonad/xmobar.hs" = {
    source = ./xmobar.hs;
    onChange = ''
      if [[ -v DISPLAY ]] ; then
          echo "Recompiling xmobar"
          xmobar -r ~/.xmonad/xmobar.hs &
          sleep 2
          disown
          killall xmobar
          echo "Restarting"
          xmonad --restart
      fi
    '';
  };
}
