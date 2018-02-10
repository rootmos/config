. ~/.profile
. ~/.bash_aliases
PS1='\u@\h:\w\$ '
export MANWIDTH=79
export GPG_TTY=$(tty)

. /home/gustav/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
