{ config, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    unstable.ranger
    unrar
    unzip
    odt2txt
    catdoc
    catdocx
  ];
  home.file.".config/ranger/commands.py".text = builtins.readFile ./commands.py;
  home.file.".config/ranger/commands_full".text = builtins.readFile ./commands_full.py;
  home.file.".config/ranger/rc.conf".text = builtins.readFile ./rc.conf;
  home.file.".config/ranger/rifle.conf".text = builtins.readFile ./rifle.conf;
  home.file.".config/ranger/scope.sh".text = builtins.readFile ./scope.sh;
}
