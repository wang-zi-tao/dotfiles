{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = {
    programs.obsidian = {
      enable = true;
      cli.enable = true;
      vaults = {
        "Personal" = {
          target = "文档/Obsidian/";
          enable = true;
        };
        "Work" = {
          target = "文档/Obsidian-work/";
          enable = false;
        };
      };
    };
  };
}
