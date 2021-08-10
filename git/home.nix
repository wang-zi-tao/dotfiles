{ pkgs, ... }: {
  programs.git = {
    enable = true;
    lfs.enable = true;
    userEmail = "2451355023@qq.com";
    userName = "wang-zi-tao";
    aliases = { diff = "diff --word-diff"; };
    extraConfig = {
      merge = { tool = "vimdiff"; };
      mergetool = {
        prompt = true;
        vimdiff = {
          cmd = ''
            nvim -d $LOCAL $MERGED $BASE $REMOTE -c 'wincmd w' -c 'wincmd J'
          '';
        };
      };
      diff = { tool = "vimdiff"; };
    };
  };
}
