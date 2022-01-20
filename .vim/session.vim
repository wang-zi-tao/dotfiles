let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +25 ~/workspace/nixos/secret/M6/id_rsa
badd +1 ~/workspace/nixos/secret/M6/id_rsa.pub
badd +144 ~/workspace/nixos/home-manager/terminal/zsh/zsh.nix
badd +23 ~/workspace/nixos/home-manager/terminal/terminal.nix
badd +1 ~/workspace/nixos/secret/aliyun-ecs/id_rsa.pub
badd +24 ~/workspace/nixos/secret/aliyun-ecs/id_rsa
badd +26 ~/workspace/nixos/home-manager/terminal/tmux/tmux.nix
badd +21 ~/workspace/nixos/home-manager/terminal/tmux/tmux.conf
badd +7 ~/workspace/nixos/machine/aliyun-ecs.nix
badd +127 ~/workspace/nixos/machine/huawei-ecs.nix
badd +54 ~/workspace/nixos/machine/MECHREV-z2-air/machine.nix
badd +8 ~/workspace/nixos/services/virtualbox.nix
badd +30 ~/workspace/nixos/services/lxd/lxd.yaml
badd +7 ~/workspace/nixos/services/lxd/net.conf
badd +43 ~/workspace/nixos/services/container.nix
badd +5 ~/workspace/nixos/module/network.nix
badd +74 ~/workspace/nixos/module/gui.nix
badd +6 ~/workspace/nixos/machine/lxd.nix
badd +72 ~/workspace/nixos/flake.nix
badd +1 ~/workspace/nixos/scripts/dockerfile_from_image.sh
badd +11 ~/workspace/nixos/scripts/password-generator.py
badd +16 ~/workspace/nixos/module/nixos.nix
badd +4 ~/workspace/nixos/module/terminal.nix
badd +11 ~/workspace/nixos/scripts/nixos-build-lxc.sh
badd +1 ~/workspace/nixos/home-manager/application/application.nix
badd +13 ~/workspace/nixos/packages/seaweedfs/default.nix
badd +1 term://~/workspace/nixos//46326:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh
badd +24 ~/workspace/nixos/machine/MECHREV-z2-air/hardware-configuration.nix
badd +143 ~/workspace/nixos/packages/wpsoffice/default.nix
badd +246 ~/workspace/nixos/home-manager/desktop/xmonad/xmonad.hs
badd +31 ~/workspace/nixos/home-manager/terminal/tmux/drop.yml
badd +1 term://~/workspace/nixos//22793:/nix/store/2lbh492826zha3s1ma8ma64a3ynz8l63-zsh-5.8/bin/zsh
badd +6 ~/workspace/nixos/home-manager/terminal/tmux/workspace-1.yml
badd +35 ~/workspace/nixos/home-manager/terminal/tmux/workspace-2.yml
badd +8 ~/workspace/nixos/home-manager/terminal/tmux/workspace-3.yml
badd +8 ~/workspace/nixos/home-manager/terminal/tmux/workspace-4.yml
badd +6 ~/workspace/nixos/home-manager/terminal/tmux/workspace-5.yml
badd +6 ~/workspace/nixos/home-manager/terminal/tmux/workspace-6.yml
badd +6 ~/workspace/nixos/home-manager/terminal/tmux/workspace-7.yml
badd +6 ~/workspace/nixos/home-manager/terminal/tmux/workspace-8.yml
badd +11 ~/workspace/nixos/home-manager/terminal/tmux/workspace-9.yml
badd +1 term://~/workspace/nixos//38194:/nix/store/2lbh492826zha3s1ma8ma64a3ynz8l63-zsh-5.8/bin/zsh
badd +1 ~/workspace/nixos/home-manager/desktop/dunst.nix
badd +19 ~/workspace/nixos/packages/layan-cursor-theme/default.nix
badd +0 term://~/workspace/nixos//26782:/nix/store/2lbh492826zha3s1ma8ma64a3ynz8l63-zsh-5.8/bin/zsh
argglobal
%argdel
edit ~/workspace/nixos/machine/MECHREV-z2-air/fs.nix
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
balt ~/workspace/nixos/packages/layan-cursor-theme/default.nix
let s:l = 24 - ((22 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 24
normal! 03|
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0&& getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOcsF
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
