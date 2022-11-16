all: 
	nix run .\#deploy-rs -- -d --fast-connection true -c
compile-all: 
self:
	nixos-rebuild --flake ".#${HOST}" --target-host root@localhost switch $(ARGS)
wangzi-pc:
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.32.128 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.16.11 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.17.11 switch $(ARGS)
wangzi-nuc:
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.32.1 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.16.12 switch $(ARGS) || \
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.17.12 switch $(ARGS)
huawei-ecs:
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch $(ARGS) || \
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@192.168.16.1 switch $(ARGS)
aliyun-hk:
	nixos-rebuild --flake '.#aliyun-hk' --target-host root@47.243.22.114 switch $(ARGS)
aliyun-ecs:
	nixos-rebuild --flake '.#aliyun-ecs' --target-host root@116.62.23.116 switch $(ARGS)
lxd:
	nixos-generate -f lxc --flake '.#lxd' $(ARGS)
	lxc image import --alias nixos `nixos-generate -f lxc-metadata` result/tarball/nixos-system-x86_64-linux.tar.xz local: --force-local --verbose
nova9: 
	nix-on-droid --flake ".#nova9" switch
M6: 
	nix-on-droid --flake ".#M6" switch
update:
	nix flake update
	make ARGS="$(ARGS)"
home:
	result=$(mktemp -d)
	trap "rm $(result)" EXIT
	os_name=$(uname)
	echo nix build ".#homeConfigurations.$(uname -m)-${os_name,,}.$(PROFILE).activation-script" --out-link $(result)/result
	echo bash $result/result/activate

