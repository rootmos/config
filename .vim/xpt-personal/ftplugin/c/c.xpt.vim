XPTemplate priority=personal

XPTvar $BRif ' '
XPTvar $BRel \n
XPTvar $BRloop ' '
XPTvar $BRfun ' '
XPTvar $author 'Gustav Behm'
XPTvar $email gustav.behm@gmail.com

XPT malloc hint=malloc
`ty^* `v^ = (`ty^*)malloc(sizeof(*`v^));

XPT calloc hint=calloc
`ty^* `v^ = (`ty^*)calloc(sizeof(*`v^), 1);

XPT uv
if(0 != (r = uv_`f^(`args^))) {
    panic("uv_`f^(...): %s\n", uv_strerror(r));
}

XPT cast hint=cast
`ty^* `v^ = (`ty^*)`w^;