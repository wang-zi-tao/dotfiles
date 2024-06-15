{ pkgs, config, ... }:
{
  home.file.".procs.toml".source = ./procs.toml;
  home.packages = with pkgs; [ procs ];
}
