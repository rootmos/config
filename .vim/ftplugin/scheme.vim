au VimEnter * syntax keyword Statement lambda conceal cchar=λ
au VimEnter * syntax keyword Statement define conceal cchar=≔
"au VimEnter * setlocal conceallevel=2

setl lispwords+=trace-lambda
syn keyword schemeSyntax trace-lambda

syn keyword schemeFunction get-line
syn keyword schemeFunction with-input-from-string

setlocal tw=79
setl modeline
