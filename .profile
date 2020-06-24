PATH=$HOME/.gem/ruby/2.5.0/bin:$PATH

for p in /home/gustav/root/*; do
    if [ -d "$p/bin" ]; then
        PATH="$p/bin:$PATH"
    fi

    if [ -d "$p/usr/bin" ]; then
        PATH="$p/usr/bin:$PATH"
    fi

    if [ -d "$p/share/man" ]; then
        MANPATH="$p/share/man:$MANPATH"
    fi
done

GOPATH=$HOME/upvest/go:$HOME/go
PATH=$HOME/upvest/go/bin:$HOME/go/bin:$PATH
PATH=$HOME/.cargo/bin:$PATH

PATH=$HOME/bin:$HOME/.local/bin:$PATH

export PLAYER=mpv
export EDITOR=vim
export VOD_DIR=$HOME/vods
