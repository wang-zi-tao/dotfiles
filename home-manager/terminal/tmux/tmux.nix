{ pkgs, ... }:
{
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
    historyLimit = 65536;
    baseIndex = 1;
    plugins =
      with pkgs.tmuxPlugins;
      with pkgs.tmuxPlugins;
      [
        {
          plugin = resurrect;
          extraConfig = "set -g @resurrect-strategy-nvim 'session'";
        }
        vim-tmux-navigator
        tmux-fzf
        fzf-tmux-url
        logging
        sysstat
        {
          plugin = yank;
          extraConfig = ''
            set -g @custom_copy_command 'xclip'
          '';
        }
        # {
        #   plugin = power-theme;
        #   extraConfig = ''
        #     # set -g @tmux_power_show_upload_speed true
        #     # set -g @tmux_power_show_download_speed true
        #     set -g @tmux_power_theme 'sky'
        #   '';
        # }
        # sensible
        {
          plugin = jump;
          extraConfig = ''
            set -g @jump-key 'd'
          '';
        }
        {
          plugin = extrakto;
          extraConfig = ''
            set -g @extrakto_key 'f'
            set -g @extrakto_copy_key 'y'
            set -g @extrakto_open_key 'o'
            set -g @extrakto_edit_key 'e'
            set -g @extrakto_open_tool 'xdg-open'
          '';
        }
        {
          plugin = continuum;
          extraConfig = ''
            set -g @continuum-save-interval '1'
            set -g @continuum-boot 'on'
            set -g @continuum-restore 'on'
          '';
        }
        net-speed
        better-mouse-mode
        cpu
      ];
    tmuxinator.enable = true;
  };
  home.packages = with pkgs; [
    xclip
    xsel
  ];
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
  programs.fzf.tmux.enableShellIntegration = true;
}
