all: 
	make this || true
	make wangzi-pc || true
	make wangzi-nuc || true
	make huawei-ecs || true
	make aliyun-hk || true
	make aliyun-ecs || true
this:
	git add . 
	nixos-rebuild --flake ".#${HOST}" --target-host root@localhost switch
wangzi-pc:
	git add . 
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.32.128 switch || \
	nixos-rebuild --flake '.#wangzi-pc' --target-host root@192.168.31.240 switch
wangzi-nuc:
	git add . 
	nixos-rebuild --flake '.#wangzi-nuc' --target-host root@192.168.16.12 switch
huawei-ecs:
	git add . 
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch
aliyun-hk:
	git add . 
	nixos-rebuild --flake '.#aliyun-hk' --target-host root@47.243.22.114 switch
aliyun-ecs:
	git add . 
	nixos-rebuild --flake '.#aliyun-ecs' --target-host root@116.23.62.116 switch

lxd:
	git add . 
	nix build '.#nixosConfigurations.lxd'
	lxc image import --alias nixos `nixos-generate -f lxc-metadata` result/tarball/nixos-system-x86_64-linux.tar.xz local: --force-local --verbose
update:
	nix flake update
	make
