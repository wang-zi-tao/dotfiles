all: 
	make self ARGS="$(ARGS)" || true
	make wangzi-pc ARGS="$(ARGS)" || true
	make wangzi-nuc ARGS="$(ARGS)" || true
	make huawei-ecs ARGS="$(ARGS)" || true
	make aliyun-hk ARGS="$(ARGS)" || true
	make aliyun-ecs ARGS="$(ARGS)" || true
self:
	nixos-rebuild --flake ".#${HOST}" --target-host root@localhost switch $(ARGS)
wangzi-pc:
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.2.8 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.16.11 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.17.11 switch $(ARGS)
wangzi-nuc:
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.2.9 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.16.12 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.17.12 switch $(ARGS)
huawei-ecs:
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch $(ARGS)
aliyun-hk:
	nixos-rebuild --flake '.#aliyun-hk' --target-host root@47.243.22.114 switch $(ARGS)
aliyun-ecs:
	nixos-rebuild --flake '.#aliyun-ecs' --target-host root@116.62.23.116 switch $(ARGS)
lxd:
	nix build '.#nixosConfigurations.lxd' $(ARGS)
	lxc image import --alias nixos `nixos-generate -f lxc-metadata` result/tarball/nixos-system-x86_64-linux.tar.xz local: --force-local --verbose
update:
	nix flake update
	make ARGS="$(ARGS)"
