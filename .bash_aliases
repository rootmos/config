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
alias gc="g c"
alias gca="g ca"
alias gs="g s"
alias ga="g a"
alias gap="g ap"
alias gr="g r"
