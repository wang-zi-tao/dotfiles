{ pkgs, ... }: {
  programs.tmux = {
    enable = true;
    clock24 = true;
    # keyMode = "vi";
    shortcut = "x";
    extraConfig = builtins.readFile ./tmux.conf;
    newSession = false;
    shell = "${pkgs.zsh}/bin/zsh";
    escapeTime = 10;
    terminal = "tmux-256color";
    plugins = (with pkgs.unstable.tmuxPlugins;
      with pkgs.tmuxPlugins; [
        {
          plugin = power-theme;
          extraConfig = ''
            # set -g @tmux_power_show_upload_speed true
            # set -g @tmux_power_show_download_speed true
            set -g @tmux_power_theme 'sky'
          '';
        }
        {
          plugin = resurrect;
          extraConfig = "set -g @resurrect-strategy-nvim 'session'";
        }
        vim-tmux-navigator
        tmux-fzf
        fzf-tmux-url
        logging
        # sysstat
        yank
        # net-speed
        better-mouse-mode
        # sidebar
      ]);
  };
}
