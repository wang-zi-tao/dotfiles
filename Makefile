all:
	git add . && sudo nixos-rebuild switch --flake .

huawei-ecs:
	nix --experimental-features 'flakes nix-command' build '.#huawei-ecs.activationPackage'
	./result/activate
aliyun-ecs:
	nix --experimental-features 'flakes nix-command' build '.#aliyun-ecs.activationPackage'
	./result/activate
