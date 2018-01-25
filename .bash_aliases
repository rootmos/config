alias u="ls"
alias ua="ls -a"
alias uu="ls -lh"

alias tm="tmux attach || tmux new"
alias zero="ssh -t zero tmux attach -t zero"

alias feh="feh -FZ"

alias g="git"
alias jävel="git"

alias master="git fetch; git checkout origin/master"
alias mäster="git fetch; git checkout origin/master"
alias pass-private="PASSWORD_STORE_DIR=~/.password-store-private/ pass"

alias aws-env='AWS_ACCESS_KEY_ID=$(aws-access-key) AWS_SECRET_ACCESS_KEY=$(aws-secret-key) AWS_DEFAULT_REGION=eu-west-1'

alias gp="gist -p -c"

#alias dcqlsh="sudo docker run --net=host -it --rm cassandra:2.0.17 cqlsh"

alias jwt="sed -e 's/.*\.\(.*\)\..*/\1/' | base64 -d 2>/dev/null | jq ."
