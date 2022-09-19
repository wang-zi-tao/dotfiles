all: 
	make self ARGS="$(ARGS)" || true
	make wangzi-pc ARGS="$(ARGS)" || true
	make wangzi-nuc ARGS="$(ARGS)" || true
	make huawei-ecs ARGS="$(ARGS)" || true
	make aliyun-hk ARGS="$(ARGS)" || true
	make aliyun-ecs ARGS="$(ARGS)" || true
compile-all: 
	nix build ".#all" --option binary-caches ''
	nixos-rebuild --flake ".#${HOST}" build $(ARGS) --option binary-caches ''
	nixos-rebuild --flake '.#wangzi-pc' build $(ARGS) --option binary-caches ''
	nixos-rebuild --flake '.#wangzi-nuc' build $(ARGS) --option binary-caches ''
	nixos-rebuild --flake '.#huawei-ecs' build $(ARGS) --option binary-caches ''
	nixos-rebuild --flake '.#aliyun-hk' build $(ARGS) --option binary-caches ''
	nixos-rebuild --flake '.#aliyun-ecs' build $(ARGS) --option binary-caches ''
compile-install-all: 
	make self ARGS="--option binary-caches '' $(ARGS)" 
	make wangzi-pc ARGS="--option binary-caches '' $(ARGS)" 
	make wangzi-nuc ARGS="--option binary-caches '' $(ARGS)" 
	make huawei-ecs ARGS="--option binary-caches '' $(ARGS)" 
	make aliyun-hk ARGS="--option binary-caches '' $(ARGS)" 
	make aliyun-ecs ARGS="--option binary-caches '' $(ARGS)"
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
	home-manager switch --flake ".#$(PROFILE)"
