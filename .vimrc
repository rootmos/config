set nocompatible
set ruler
"set number
"set relativenumber
"autocmd BufEnter * set relativenumber
set showcmd
set laststatus=2

let loaded_matchparen = 1

set wildmode=longest,list,full
set wildmenu

set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'
"Bundle 'bling/vim-bufferline'
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdtree'
Bundle 'Valloric/YouCompleteMe'
"Bundle 'kien/ctrlp.vim'
"Bundle 'mileszs/ack.vim'
Bundle 'derekwyatt/vim-fswitch'
Bundle 'jlanzarotta/bufexplorer'
Bundle 'L9'
Bundle 'FuzzyFinder'
call vundle#end()

filetype plugin indent on

set textwidth=79      " Break lines at this width
set colorcolumn=+1    " Show vertical line after the textwidth

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


"noremap <F5> :w<CR>:make<CR>

noremap <F5> :bp<CR>
noremap <F6> :bn<CR>
noremap <F7> :bdelete<CR>
noremap <F8> :BufExplorer<CR>

noremap <F9> :Explore<CR>
noremap <F10> :FSHere<CR>

autocmd FileType netrw nmap <silent> <buffer> q :bdelete<CR>

inoremap hh <Esc>
inoremap uu <Esc>
nnoremap q <NOP>

let mapleader = ","
"nmap <leader>b :CtrlPBuffer<CR>
"vmap <leader>b :CtrlPBuffer<CR>
"nmap <leader>f :CtrlP<CR>
"vmap <leader>f :CtrlP<CR>

nmap <leader>b :FufBuffer<CR>
nmap <leader>f :FufFileWithCurrentBufferDir<CR>
nmap <leader>l :FufLine<CR>
nmap <leader>c :FufChange<CR>
nmap <leader>j :FufJump<CR>

command FixTrailing execute ':%s/\s\+$//c'

let g:bufExplorerDisableDefaultKeyMapping = 1


let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_global_conf.py'
"let g:ycm_filetype_whitelist = { 'cpp': 1 }
