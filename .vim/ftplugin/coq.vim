set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
nmap <leader>. :SlimeSend1 .<cr>

set noautochdir

call coquille#FNMapping()
nmap <leader>r :CoqToCursor<cr>

setlocal foldmethod=expr
setlocal foldexpr=GetPotionFold(v:lnum)

function! CoqFold(lnum)
  return '0'
endfunction
