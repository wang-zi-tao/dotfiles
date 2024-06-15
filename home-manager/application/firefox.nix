{ pkgs, config, ... }:
{
  home.packages = with pkgs; [ firefox ];
}
