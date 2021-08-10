{ pkgs, ... }: {
  programs.tmux = {
    enable = true;
    clock24 = true;
    # keyMode = "vi";
    shortcut = "x";
    extraConfig = builtins.readFile ./tmux.conf;
    newSession = false;
    shell = "${pkgs.zsh}/bin/zsh";
    # terminal = "alacritty";
    plugins = (with pkgs.tmuxPlugins; [
      {
        plugin = resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      tmux-fzf
      logging
      sysstat
      yank
      net-speed
      {
        plugin = power-theme;
        extraConfig = ''
          # set -g @tmux_power_show_upload_speed true
          # set -g @tmux_power_show_download_speed true
          set -g @tmux_power_theme 'sky'
        '';
      }
    ]);
  };
}
