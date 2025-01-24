alias ls='ls --color=auto'
alias u="ls"
alias c="continuously --"
alias e="edit"

alias g="git"

alias cal="/usr/bin/cal -m"

alias pl="playlist"

alias yay="/usr/bin/yay --sudoflags=-A"

tmp() {
    TMP=/tmp/$(d)
    mkdir -p "$TMP"
    cd "$TMP"
}

alias s="systemctl"
alias su="systemctl --user"
alias j="journalctl"
alias ju="journalctl --user"

# missepllings
alias ga="g a"
alias gap="g ap"
alias gc="g c"
alias gca="g ca"
alias gd="g d"
alias gdd="g dd"
alias gp="g p"
alias gpb="g pb"
alias gpp="g pp"
alias gr="g r"
alias gs="g s"
alias gss="g ss"
