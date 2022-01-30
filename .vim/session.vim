let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +26 ~/workspace/nixos/home-manager/terminal/terminal.nix
badd +13 ~/workspace/nixos/machine/MECHREV-z2-air/machine.nix
badd +18 ~/workspace/nixos/services/container.nix
badd +30 ~/workspace/nixos/module/network.nix
badd +10 ~/workspace/nixos/module/gui.nix
badd +36 ~/workspace/nixos/flake.nix
badd +25 ~/workspace/nixos/module/nixos.nix
badd +3 ~/workspace/nixos/module/terminal.nix
badd +74 ~/workspace/nixos/home-manager/develop/neovim/init.vim
badd +0 term://~/workspace/nixos//2486096:/nix/store/2lbh492826zha3s1ma8ma64a3ynz8l63-zsh-5.8/bin/zsh
badd +9 ~/workspace/nixos/machine/MECHREV-z2-air/network.nix
badd +1 ~/workspace/nixos/secret/huawei-ecs/network.nix
badd +7 ~/workspace/nixos/secret/huawei-ecs/wireguard.nix
badd +2 ~/workspace/nixos/services/gpaste.nix
badd +15 ~/workspace/nixos/services/lightdm.nix
badd +7 ~/workspace/nixos/secret/wangzi-pc/wireguard.nix
badd +112 ~/workspace/nixos/module/cluster.nix
badd +31 ~/workspace/nixos/module/user.nix
badd +8 ~/workspace/nixos/services/sshd.nix
badd +1 ~/workspace/nixos/secrets/public-key.yaml
badd +2 ~/workspace/nixos/module/libinput.nix
badd +16 ~/workspace/nixos/module/sshd.nix
badd +22 ~/workspace/nixos/module/container.nix
badd +2 ~/workspace/nixos/module/virtualbox.nix
badd +2 ~/workspace/nixos/module/lightdm.nix
badd +1 ~/workspace/nixos/module/gunpg.nix
badd +4 ~/workspace/nixos/module/gpaste.nix
badd +23 ~/workspace/nixos/module/boot.nix
badd +13 ~/workspace/nixos/shell.nix
badd +24 ~/workspace/nixos/module/lxd/lxd.yaml
badd +4 ~/workspace/nixos/module/mySQL.nix
argglobal
%argdel
edit term://~/workspace/nixos//2486096:/nix/store/2lbh492826zha3s1ma8ma64a3ynz8l63-zsh-5.8/bin/zsh
argglobal
let s:l = 1 - ((0 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0&& getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOcsF
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
