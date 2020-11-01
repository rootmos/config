call TabsVsSpaces(2)
nmap <leader>. :SlimeSend1 .<cr>

setlocal noautochdir

setlocal foldmethod=expr
setlocal foldexpr=GetPotionFold(v:lnum)

function! CoqFold(lnum)
  return '0'
endfunction
