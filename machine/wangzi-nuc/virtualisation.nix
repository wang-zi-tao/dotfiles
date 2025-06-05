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
      networks = lib.mkForce [
        {
          definition = ''
            <network>
              <name>default</name>
              <bridge name='virbr0'/>
              <forward/>
              <ip address='192.168.122.1' netmask='255.255.255.0'>
              </ip>
              <ip family="ipv6" address="2001:db8:ca2:2::1" prefix="64">
              </ip>
            </network>
          '';
          active = true;
        }
      ];
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
