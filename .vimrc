scriptencoding utf-8
set encoding=utf-8

set nocompatible
set modeline
set ruler
set number
set showcmd
set laststatus=2
set clipboard+=unnamed
set clipboard+=unnamedplus
set backspace=indent,eol,start
"set autochdir

set nowrap

set showmatch
set matchtime=3

set wildmode=longest,list,full
set wildmenu
set wildignore=target,**/META-INF/**,**/build/**,*class,*.orig,**/_build/**,*.native,*.byte,**/_build/**,*.vo,*.glob,*.v.d,*.aux

filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-fugitive'
Bundle 'Valloric/YouCompleteMe'
"Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-fswitch'
Bundle 'jlanzarotta/bufexplorer'
Bundle 'L9'
Bundle 'FuzzyFinder'
"Bundle 'raichoo/haskell-vim'
Plugin 'neovimhaskell/haskell-vim.git'
Bundle 'derekwyatt/vim-scala'
Plugin 'rootmos/ack.vim'
Bundle 'wincent/command-t'
Bundle 'drmingdrmer/xptemplate'
Plugin 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-abolish'
"Bundle 'mpollmeier/vim-scalaConceal'
Bundle 'godlygeek/tabular'
Bundle 'IN3D/vim-raml'
Plugin 'rust-lang/rust.vim'
Plugin 'ngn/vim-apl'
Plugin 'rootmos/vim-slime'
"Plugin 'tounaishouta/coq.vim'
Plugin 'bluelightning32/coquille'
Plugin 'let-def/vimbufsync'
Plugin 'idris-hackers/idris-vim.git'
Plugin 'guersam/vim-j'
"Plugin 'kovisoft/paredit'
Plugin 'luochen1990/rainbow'
"Plugin 'kien/rainbow_parentheses.vim'
Plugin 'tomlion/vim-solidity'
call vundle#end()

" Import my xpt templates
set runtimepath+=~/.vim/xpt-personal
let g:xptemplate_key = '<C-t>'
let g:xptemplate_nav_next = '<C-t>'

filetype plugin indent on

set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

set list
set listchars=tab:»\ 

set colorcolumn=+1
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/

syntax enable

set ignorecase
set smartcase
set incsearch
set hlsearch

colorscheme solarized
set t_Co=256
call togglebg#map("<F12>")

if has("gui_running")
	set guioptions-=m
	set guioptions-=T

	set background=light

    if has("gui_gtk2")
		set guifont=Inconsolata\ 12
	elseif has("gui_win32")
		set guifont=Consolas:h11:cANSI
	endif
else
    set background=light
endif

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>


"noremap <F5> :w<CR>:make<CR>

noremap <F5> :bp<CR>
noremap <F6> :bn<CR>
noremap <F7> :bdelete<CR>
noremap <F8> :BufExplorer<CR>

noremap <F9> :Explore<CR>
noremap <F10> :FSHere<CR>

autocmd FileType netrw nmap <silent> <buffer> q :bdelete<CR>

inoremap hh <Esc>
inoremap uu <Esc>:w<CR>
nnoremap q <NOP>
nnoremap Q <NOP>

nnoremap <C-W><C-W> <NOP>

let mapleader = ","
let maplocalleader = '-'

nmap <leader>b :FufBuffer<CR>
nmap <leader>f :FufFileWithCurrentBufferDir<CR>
nmap <leader>c :FufChange<CR>
nmap <leader>j :FufJump<CR>
nmap <leader>t :CommandT<CR>

imap <C-l> λ
imap <C-a> α
imap <C-b> ⊥
imap <C-j> ∘

command! FixTrailing execute ':%s/\s\+$//c'

let g:bufExplorerDisableDefaultKeyMapping = 1
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 1

"let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_global_conf.py'
"let g:ycm_filetype_whitelist = { 'cpp': 1 }

let g:ackprg = 'ag --nogroup --nocolor --column --ignore=META-INF'
nnoremap <F4> :Ack 
nnoremap <F3> :AckWord<CR>
command! -bang AckWord call ack#Ack('grep<bang>', "'\\b" . expand("<cword>") . "\\b'")

nmap ^[[19^ :wqall<CR>
imap ^[[19^ <Esc>:wqall<CR>
nmap <F8> :w<CR>
imap <F8> <Esc>:w<CR>

nmap <leader>n :cn<CR>
nmap <leader>p :cp<CR>
nmap <leader>o :copen<CR>
nmap <leader>q :cclose<CR>

let g:slime_target = "tmux"
nmap <c-c><c-l> :SlimeSendCurrentLine<cr>
nmap <c-c><c-d> :SlimeSendCurrentLine<cr>

nmap <leader>C :source ~/.vimrc<cr>

:call has('python')

function! Note(offset)
    let dir = $NOTES_DIR
    if dir == ""
        let dir = "~/notes"
    endif

    if empty(glob(dir))
        call system("mkdir -p " . dir)
        call system("git init " . dir)
    endif

    let offset = a:offset
    if a:offset == ""
        let offset = 0
    endif

    let date = system('date +%F --date="' . offset . ' day"')[:-2]
    let fn = date . ".md"
    execute "e " . dir . "/" . fn
    execute 'autocmd BufWritePost <buffer> execute "silent !(cd ' dir ' && git add ' fn ' && git commit --no-gpg-sign --message=\"Updated note:' date . '\" -- ' fn ' && git push)" | redraw!'
endfunction

command! -nargs=? Note call Note(<q-args>)


if executable('opam')
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  if executable('ocamlmerlin')
    execute 'set rtp+=' . g:opamshare . '/merlin/vim'
    let g:merlin_split_method = 'vertical'
    let g:syntastic_ocaml_checkers = ['merlin']
    nmap <buffer> <C-]> :MerlinLocate<CR>
  endif
  if executable('ocp-indent')
    execute 'set rtp+=' . g:opamshare . '/ocp-indent/vim'
  endif
  if executable('ocp-index')
    execute 'set rtp+=' . g:opamshare . '/ocp-index/vim'
  endif
endif
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
