call TabsVsSpaces(2)
setlocal tw=79

nmap <buffer> <leader>, :MerlinTypeOf<CR>
nmap <buffer> <leader>. :MerlinLocate<CR>
nmap <buffer> <leader>n :MerlinGrowEnclosing<CR>
nmap <buffer> <leader>p :MerlinShrinkEnclosing<CR>
