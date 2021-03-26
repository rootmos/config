GOPATH=$HOME/go
PATH=$HOME/go/bin:$PATH
PATH=$HOME/.cargo/bin:$PATH
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

    if [ -d "$p/lib/pkgconfig" ]; then
        if [ -z "${PKG_CONFIG_PATH-}" ]; then
            export PKG_CONFIG_PATH="$p/lib/pkgconfig"
        else
            export PKG_CONFIG_PATH="$p/lib/pkgconfig:$PKG_CONFIG_PATH"
        fi
    fi
done

PATH=$HOME/bin:$HOME/.local/bin:$PATH

export PLAYER=play
export VIEWER=view
export EDITOR=vim

export VOD_DIR=/stash/Vods

if [ -e "/usr/local/bin/sudo-pass.askpass" ]; then
    export SUDO_ASKPASS=/usr/local/bin/sudo-pass.askpass
    alias sudo="/usr/bin/sudo -A"
fi
