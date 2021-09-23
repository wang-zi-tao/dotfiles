all:
	git add . && sudo nixos-rebuild switch --flake .

huawei-cloud-ecs-623a:
	nix --experimental-features 'flakes nix-command' build '.#huawei-cloud-ecs-623a.activationPackage'
	./result/activate
