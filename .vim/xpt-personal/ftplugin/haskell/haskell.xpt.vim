XPTemplate priority=personal

XPTvar $BRif ' '
XPTvar $BRel \n
XPTvar $BRloop ' '
XPTvar $BRfun ' '
XPTvar $author 'Gustav Behm'
XPTvar $email gustav.behm@gmail.com

XPT spec_ hint=Create\ a\ spec\ function
spec_`fun^ :: SpecWith ()
spec_`fun^ = describe "`fun^" $ do
  `cursor^
