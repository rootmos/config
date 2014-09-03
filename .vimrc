set tags=$EP_WORKSPACE/tags,$EP_WORKSPACE/tsp_tags
set tagstack
set nocompatible
set ruler
set number
set relativenumber
"autocmd BufEnter * set relativenumber
set showcmd
set laststatus=2
set autochdir
set backspace=eol,start

set wildmode=longest,list,full
set wildmenu

set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

filetype off
let runtimepath='/local/scratch/egusbeh/vim/'
set runtimepath+=/local/scratch/egusbeh/vim/bundle/Vundle.vim
call vundle#begin('/local/scratch/egusbeh/vim/bundle')
Plugin 'gmarik/Vundle.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-fugitive'
Bundle 'Valloric/YouCompleteMe'
Bundle 'derekwyatt/vim-fswitch'
Bundle 'jlanzarotta/bufexplorer'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'godlygeek/tabular'
Bundle 'rootmos/vim-journal'
call vundle#end()

filetype plugin indent on

set tabstop=3 softtabstop=3 shiftwidth=3 expandtab

set list listchars=tab:>-

"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/

syntax enable

set ignorecase
set smartcase
set incsearch
set hlsearch

set background=dark
colorscheme solarized
call togglebg#map("<F12>")
"set t_Co=256
"let g:solarized_termcolors= 256
"let g:solarized_termtrans = 1
"let g:solarized_degrade = 1
"let g:solarized_bold = 1
"let g:solarized_underline = 1
"let g:solarized_italic = 1
"let g:solarized_contrast = "normal"

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>


"noremap <F5> :w<CR>:make<CR>

noremap <F3> g]
noremap <F4> :!lvims 
noremap <F5> :bp<CR>
noremap <F6> :bn<CR>
noremap <F7> :bdelete<CR>
noremap <F8> :BufExplorer<CR>

noremap <F9> :Explore<CR>
noremap <F10> :FSHere<CR>

autocmd FileType netrw nmap <silent> <buffer> q :bdelete<CR>

inoremap hh <Esc>
nnoremap q <NOP>

let mapleader = ","

nmap <leader>b :FufBuffer<CR>
nmap <leader>f :FufFileWithCurrentBufferDir<CR>
nmap <leader>l :FufLine<CR>
nmap <leader>c :FufChange<CR>
nmap <leader>j :FufJump<CR>

nmap <leader>n  ]c
nmap <leader>p  [c

command FixTrailing execute ':%s/\s\+$//c'

let g:bufExplorerDisableDefaultKeyMapping = 1
let g:ycm_collect_identifiers_from_tags_files = 1

let g:ycm_global_ycm_extra_conf = '/local/scratch/egusbeh/vim/ycm_global_conf.py'
"let g:ycm_filetype_whitelist = { 'cpp': 1 }
