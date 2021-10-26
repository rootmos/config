augroup filetypedetect
  au BufRead,BufNewFile *?Script.sml let maplocalleader = "," | source /home/gustav/git/hol-hack/HOL/tools/vim/hol.vim
  "Uncomment the line below to automatically load Unicode
  "au BufRead,BufNewFile *?Script.sml source /home/gustav/git/hol-hack/HOL/tools/vim/holabs.vim

  au BufNewFile,BufRead *.if setf sillyif
augroup END
