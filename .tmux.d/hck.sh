tmux move-window -t 0
tmux new-window -d -n "v" -c "$HOME/lists"
tmux new-window -d -n "p"
tmux new-window -d

wifi-fix && read
