{ pkgs, ... }:
let nvr = "${pkgs.neovim-remote}/bin/nvr";
in {
  programs.git = {
    enable = true;
    lfs.enable = true;
    userEmail = "2451355023@qq.com";
    userName = "wang-zi-tao";
    aliases = { diff = "diff --word-diff"; };
    extraConfig = {
      diff = { tool = "nvr"; };
      difftool = {
        promt = true;
        nvr = {
          cmd = ''
            ${nvr} -s -d $LOCAL $REMOTE
          '';
        };
      };
      merge = { tool = "nvr"; };
      mergetool = {
        prompt = true;
        nvr = {
          cmd = ''
            ${nvr} -s -d $LOCAL $MERGED $BASE $REMOTE -c 'wincmd w' -c 'wincmd J|wincmd ='
          '';
        };
      };
    };
  };
  home.packages = with pkgs; [ delta ];
}
