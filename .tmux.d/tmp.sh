tmux move-window -t 0

export TMP=/tmp/$(d)
mkdir -p "$TMP"

tmux new-window -c "$TMP"
