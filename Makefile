all:
	git add . && sudo nixos-rebuild switch --flake .

huawei-ecs:
	nixos-rebuild --flake '.#huawei-ecs' --target-host root@139.9.235.87 switch
aliyun-ecs:
	nix --experimental-features 'flakes nix-command' build '.#aliyun-ecs.activationPackage'
	./result/activate
