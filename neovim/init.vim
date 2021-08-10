"source ~/.nix-profile/SpaceVim/init.vim

"===defx==========================================================================
" init.vim --- Entry file for neovim
" Copyright (c) 2016-2020 Wang Shidong & Contributors
" Author: Wang Shidong < wsdjeg@outlook.com >
" URL: https://spacevim.org
" License: GPLv3
"=============================================================================


" set timeoutlen=200

map <leader>h <C-w>h
map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>l <C-w>l
map <leader>v <C-w>v
map <leader>s <C-w>s
map <leader>qt :q<CR>
map <leader>qq :wqa!<CR>
map <leader>n :n<CR>
map <leader>N :N<CR>
map <leader>w :w<CR>
map [SPC]lf :Cocfix<CR>
call SpaceVim#custom#SPC('nnoremap', ['e', 'f'], 'CocFix & w', 'fix', 1)
hi Pmenu ctermfg=15 ctermbg=16  guibg=#444444
" hi PmenuSel ctermfg=7 ctermbg=4 guibg=#daf0f4 guifg=#ffffff


set noswapfile
" auto save
let autosave = 1
let g:auto_save_events = ["InsertLeave", "TextChanged", "TextChangedI", "CursorHoldI", "CompleteDone"]
" set spell
set linebreak
set wrap
set autowrite
" autocmd InsertLeave,TextChanged,FocusLost,BufLeave * silent! update
autocmd FocusLost,BufLeave * silent! update
let g:NERDTreeHidden=1

" Plugin 'ryanoasis/vim-devicons'
set guifont=DroidSansMono_Nerd_Font:h11
set autoread
set autowrite
set autowriteall
let g:auto_save = 0
let g:auto_save_events = ["InsertLeave", "TextChanged"]
" install third-party plugins
call dein#add('ryanoasis/vim-devicons')
call dein#add('tiagofumo/vim-nerdtree-syntax-highlight')
call dein#add('scrooloose/nerdtree-project-plugin')
call dein#add('PhilRunninger/nerdtree-visual-selection')

call dein#add('luochen1990/rainbow')
call dein#add('junegunn/fzf.vim')
let g:rainbow_active = 1

" call dein#add('h-youhei/vim-ibus')
" let g:ibus#layout = 'xkb:us::eng'
" let g:ibus#engine = 'libpinyin'

call dein#add('glacambre/firenvim', { 'hook_post_update': { _ -> firenvim#install(0) } })

" coc
" inoremap <silent><expr> <TAB>
      " \ pumvisible() ? "\<C-n>" :
      " \ <SID>check_back_space() ? "\<TAB>" :
      " \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" inoremap <silence><expr> <c-i> coc#refresh()
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
call SpaceVim#custom#SPC('nnoremap', ['f', 't'], 'CocCommand explorer', 'explorer', 1)

nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>

noremap ;a 1
noremap ;s 2
noremap ;d 3
noremap ;f 4
noremap ;g 5
noremap ;h 6
noremap ;j 7
noremap ;k 8
noremap ;l 9
noremap ;; 10

colorscheme onedark


set foldmethod=syntax
" autocmd BufLeave  :wa<CR>
" autocmd BufLeave  :wa<CR>
function! s:on_start() abort
  if argc() == 0
    Defx
    " if filereadable(".vim/session.vim")
      " source .vim/session.vim
    " endif
    " if filereadable(".vim/viminfo.viminfo")
      " rviminfo .vim/viminfo.viminfo
    " endif
    execute "CocCommand explorer"
  endif
endfunction
autocmd VimEnter * call s:on_start()
function! s:on_add_buffer() abort
  " TagbarOpen
endfunction
autocmd BufCreate * call s:on_add_buffer()
set sessionoptions=buffers
function! s:on_exit() abort
  if argc() == 0
    " mksession .vim/session.vim
    " wviminfo .vim/viminfo.viminfo
  endif
endfunction
autocmd VimLeave * call s:on_exit()


" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

let t:is_transparent = 1
