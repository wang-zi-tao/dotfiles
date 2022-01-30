{ pkgs, ... }: {
  programs.tmux = {
    enable = true;
    clock24 = true;
    # keyMode = "vi";
    shortcut = "x";
    extraConfig = builtins.readFile ./tmux.conf;
    newSession = true;
    shell = "${pkgs.zsh}/bin/zsh";
    escapeTime = 10;
    terminal = "tmux-256color";
    historyLimit = 16384;
    baseIndex = 1;
    plugins = (with pkgs.unstable.tmuxPlugins;
      with pkgs.tmuxPlugins; [
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
        {
          plugin = yank;
          extraConfig = ''
            set -g @custom_copy_command '${pkgs.xclip}/bin/xclip'
          '';
        }
        {
          plugin = power-theme;
          extraConfig = ''
            # set -g @tmux_power_show_upload_speed true
            # set -g @tmux_power_show_download_speed true
            set -g @tmux_power_theme 'sky'
          '';
        }
        # net-speed
        better-mouse-mode
        # sidebar
      ]);
    tmuxinator.enable = true;
  };
  home.packages = with pkgs; [ xclip xsel ];
  home.file.".config/tmuxinator/drop.yml".source = ./drop.yml;
  home.file.".config/tmuxinator/workspace-1.yml".source = ./workspace-1.yml;
  home.file.".config/tmuxinator/workspace-2.yml".source = ./workspace-2.yml;
  home.file.".config/tmuxinator/workspace-3.yml".source = ./workspace-3.yml;
  home.file.".config/tmuxinator/workspace-4.yml".source = ./workspace-4.yml;
  home.file.".config/tmuxinator/workspace-5.yml".source = ./workspace-5.yml;
  home.file.".config/tmuxinator/workspace-6.yml".source = ./workspace-6.yml;
  home.file.".config/tmuxinator/workspace-7.yml".source = ./workspace-7.yml;
  home.file.".config/tmuxinator/workspace-8.yml".source = ./workspace-8.yml;
  home.file.".config/tmuxinator/workspace-9.yml".source = ./workspace-9.yml;
}
