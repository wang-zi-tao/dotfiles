{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = lib.mkIf config.cluster.nodeConfig.shell.enable {
    programs.zsh.enable = true;
    programs.iotop.enable = true;
    environment.systemPackages =
      with pkgs;
      [
        nix-direnv
        uutils-coreutils
        pciutils
        xclip
        extra-container
      ]
      ++ (with pkgs.tmuxPlugins; [
        resurrect
        yank
        power-theme
        continuum
      ])
      ++ (lib.mapAttrsToList (
        remoteHostName: cfg:
        (pkgs.writeScriptBin "ssh-${remoteHostName}" (
          let
            make_command = user: ''
              if (( "$#" == 0 )); then
                exec ssh "${user}@${remoteHostName}.wg" -X -Y -t "zellij attach ssh -c"
              else
                exec ssh "${user}@${remoteHostName}.wg" -X -Y -t $@
              fi
            '';
          in
          if (builtins.hasAttr "wangzi" cfg.users) then make_command "wangzi" else make_command "root"
        ))
      ) config.cluster.nodes);
    programs.tmux = {
      enable = true;
      clock24 = true;
      keyMode = "vi";
      shortcut = "x";
      newSession = true;
      customPaneNavigationAndResize = true;
      escapeTime = 10;
      terminal = "tmux-256color";
      historyLimit = 16384;
      baseIndex = 1;
      plugins = with pkgs.tmuxPlugins; [
        resurrect
        vim-tmux-navigator
        tmux-fzf
        fzf-tmux-url
        logging
        sysstat
        better-mouse-mode
        # sidebar
        cpu
      ];
      extraConfig = with pkgs.tmuxPlugins; ''
        # mouse support
        set -g mouse on

        bind-key '\' split-window -h
        bind-key - split-window -v
        bind-key = new-window

        bind-key -r j select-pane -D
        bind-key -r k select-pane -U
        bind-key -r h select-pane -L
        bind-key -r l select-pane -R

        bind-key -r J resize-pane -D 5
        bind-key -r K resize-pane -U 5
        bind-key -r H resize-pane -L 5
        bind-key -r L resize-pane -R 5

        bind-key -r v copy-mode

        bind-key -r Tab last-window
        bind-key -r PageDown next-window
        bind-key -r PageUp previous-window

        # bind P paste-buffer
        # bind-key -t vi-copy 'v' begin-selection
        # bind-key -t vi-copy 'y' copy-selection
        # bind-key -t vi-copy 'r' rectangle-toggle
        # bind -n C-h run "($is_vim && tmux send-keys C-h) || \
                                  # tmux select-pane -L"
        #
        # bind -n C-j run "($is_vim && tmux send-keys C-j)  || \
                                 # ($is_fzf && tmux send-keys C-j) || \
                                 # tmux select-pane -D"
        #
        # bind -n C-k run "($is_vim && tmux send-keys C-k) || \
                                  # ($is_fzf && tmux send-keys C-k)  || \
                                  # tmux select-pane -U"
        #
        # bind -n C-l run  "($is_vim && tmux send-keys C-l) || \
                                  # tmux select-pane -R"

        # Turn off status bar
        # set -g status off

        # Turn on window titles, so that it's titled `vim', `weechat', etc
        set -g set-titles on
        set -g set-titles-string '#W'
        set-window-option -g automatic-rename on
        set -g history-limit 8192
        set-option -sa terminal-overrides ',alacritty:RGB'
        # set-option -ga terminal-overrides ",xterm-256color:Tc"
        set -g default-terminal "screen-256color"

        set -g @resurrect-strategy-nvim 'session'
        run-shell ${resurrect.rtp}

        set -g @custom_copy_command 'xclip'
        run-shell ${yank.rtp}

        set -g @tmux_power_upload_speed_icon ' '
        set -g @tmux_power_download_speed_icon ' '
        set -g @tmux_power_show_upload_speed true
        set -g @tmux_power_show_download_speed true
        set -g @tmux_power_theme 'sky'
        run-shell ${power-theme.rtp}

        run-shell ${net-speed.rtp}

        set -g @continuum-restore 'off'
        run-shell ${continuum.rtp}
      '';
    };
  };
}
