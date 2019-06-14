XPTemplate priority=personal

XPTvar $BRif ' '
XPTvar $BRel \n
XPTvar $BRloop ' '
XPTvar $BRfun ' '
XPTvar $author 'Gustav Behm'
XPTvar $email gustav.behm@gmail.com

XPT self. hint=init
self.`v^ = `v^

XPT class hint=class
class `C^:
    def __init__(self, `v^):
        `cursor^
