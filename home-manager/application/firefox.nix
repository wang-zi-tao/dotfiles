{pkgs,config,...}:{
  programs.firefox = {
    enable = true;
    # package = pkgs.unstable.firefox;
  };
}
