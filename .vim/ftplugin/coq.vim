call TabsVsSpaces(2)
nmap <leader>. :SlimeSend1 .<cr>

setlocal noautochdir

call coquille#FNMapping()
nmap <leader>r :CoqToCursor<cr>

setlocal foldmethod=expr
setlocal foldexpr=GetPotionFold(v:lnum)

function! CoqFold(lnum)
  return '0'
endfunction
