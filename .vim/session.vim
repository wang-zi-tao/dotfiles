let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +231 ~/workspace/nixos/home-manager/desktop/xmonad/xmonad.hs
badd +60 ~/workspace/nixos/module/gui.nix
badd +1 ~/workspace/nixos/module/libinput.nix
badd +0 term://~/workspace/nixos//36950:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh
argglobal
%argdel
edit ~/workspace/nixos/module/libinput.nix
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
split
1wincmd k
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
wincmd =
argglobal
balt ~/workspace/nixos/module/gui.nix
let s:l = 1 - ((0 * winheight(0) + 12) / 25)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
argglobal
if bufexists("term://~/workspace/nixos//36950:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh") | buffer term://~/workspace/nixos//36950:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh | else | edit term://~/workspace/nixos//36950:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh | endif
if &buftype ==# 'terminal'
  silent file term://~/workspace/nixos//36950:/nix/store/qc30xims1kxpv0waghc0cf1gqn6zdmsn-zsh-5.8/bin/zsh
endif
balt ~/workspace/nixos/module/libinput.nix
let s:l = 1 - ((0 * winheight(0) + 13) / 26)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
2wincmd w
wincmd =
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
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
