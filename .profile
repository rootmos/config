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

PATH=$HOME/.dotnet/tools:$PATH

export LUA_PATH='/usr/share/lua/5.4/?.lua;/usr/share/lua/5.4/?/init.lua;/usr/lib/lua/5.4/?.lua;/usr/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua;/home/gustav/.luarocks/share/lua/5.4/?.lua;/home/gustav/.luarocks/share/lua/5.4/?/init.lua'
export LUA_CPATH='/usr/lib/lua/5.4/?.so;/usr/lib/lua/5.4/loadall.so;./?.so;/home/gustav/.luarocks/lib/lua/5.4/?.so'
PATH=$HOME/.luarocks/bin:$PATH

systemctl --user import-environment PATH

export PLAYER=play
export VIEWER=view
export EDITOR=edit

complete -A file view

if [ -e "/usr/local/bin/sudo-pass.askpass" ]; then
    export SUDO_ASKPASS=/usr/local/bin/sudo-pass.askpass
    alias sudo="/usr/bin/sudo -A"
fi

export REQUESTS_CA_BUNDLE=/etc/ssl/cert.pem
