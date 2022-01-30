all: 
	git add .
	sudo nixos-rebuild switch --flake .
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch
wangzi-pc:
	git add . 
	sudo nixos-rebuild switch --flake .
huawei-ecs:
	git add . 
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch
aliyun-ecs:
	git add . 
	nix --experimental-features 'flakes nix-command' build '.#aliyun-ecs.activationPackage'
	./result/activate

lxd:
	git add . 
	nix build '.#nixosConfigurations.lxd'
	lxc image import --alias nixos `nixos-generate -f lxc-metadata` result/tarball/nixos-system-x86_64-linux.tar.xz local: --force-local --verbose
