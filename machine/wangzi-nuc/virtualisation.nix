{
  config,
  pkgs,
  lib,
  NixVirt,
  ...
}:
{
  config = {
    virtualisation.libvirt.swtpm.enable = true;
    virtualisation.libvirt.connections."qemu:///session" = {
      domains = [
        {
          definition = NixVirt.lib.domain.writeXML (
            NixVirt.lib.domain.templates.windows {
              name = "win11";
              uuid = "6933672e-aef1-11ef-8986-a78911e7c48c";
              memory = {
                count = 8;
                unit = "GiB";
              };
              storage_vol = {
                pool = "home";
                volume = "win11.qcow2";
              };
              install_vol = /home/wangzi/workspace/nixos/windows/iso/Win11_24H2_Chinese_Simplified_x64.iso;
              nvram_path = /home/wangzi/vm/win11/win11.nvram;
              virtio_net = true;
              virtio_drive = true;
              install_virtio = true;
            }
          );
        }
      ];
    };
  };
}
