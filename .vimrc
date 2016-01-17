scriptencoding utf-8
set encoding=utf-8

set nocompatible
set nomodeline
set ruler
set number
set relativenumber
"autocmd BufEnter * set relativenumber
set showcmd
set laststatus=2
set clipboard+=unnamed
set clipboard+=unnamedplus
set backspace=indent,eol,start
set autochdir

let loaded_matchparen = 1

set wildmode=longest,list,full
set wildmenu

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
Bundle 'raichoo/haskell-vim'
Bundle 'derekwyatt/vim-scala'
Plugin 'rootmos/ack.vim'
Plugin 'wincent/command-t'
call vundle#end()

filetype plugin indent on

set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

set list
set listchars=tab:»\ 

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
    set background=dark
endif

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>


set makeprg=in_root\ mvn\ -q
set errorformat=\[ERROR]\ %f:[%l\\,%v]\ %m
noremap <F5> :make compile<CR>

noremap <F9> :make test<CR>
noremap <F10> :make verify<CR>

noremap <F2> :!echo; echo "Spawning shell from inside vim..."; bash<CR>
noremap <F7> :bdelete<CR>

autocmd FileType netrw nmap <silent> <buffer> q :bdelete<CR>

inoremap hh <Esc>
inoremap uu <Esc>
nnoremap q <NOP>

let mapleader = ","

nmap <leader>b :FufBuffer<CR>
nmap <leader>f :FufFileWithCurrentBufferDir<CR>
nmap <leader>l :FufLine<CR>
nmap <leader>c :FufChange<CR>
nmap <leader>j :FufJump<CR>
nmap <leader>t :CommandT<CR>

command FixTrailing execute ':%s/\s\+$//c'

let g:bufExplorerDisableDefaultKeyMapping = 1
let g:ycm_collect_identifiers_from_tags_files = 1

let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_global_conf.py'
"let g:ycm_filetype_whitelist = { 'cpp': 1 }

let g:ackprg = 'ag --nogroup --nocolor --column --ignore=META-INF'
noremap <F4> :Ack 
noremap <F3> :AckWord<CR>
command! -bang AckWord call ack#Ack('grep<bang>', '\b' . expand("<cword>") . '\b')

nmap <leader>n :cn<CR>
nmap <leader>p :cp<CR>
nmap <leader>o :copen<CR>
nmap <leader>q :cclose<CR>
