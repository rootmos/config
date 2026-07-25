RUTIN="$HOME/git/rutin"
tmux move-window -t 0
tmux new-window -n "klocka" -c "$RUTIN"
tmux split-window -d -v -c "$RUTIN"
tmux split-window -d -h -c "$RUTIN" -t 1

tmux send-keys -t 1 "C-l" "klocka "
tmux send-keys -t 2 "C-l" "progress add "
tmux send-keys -t 3 "./watch.sh" Enter

tmux select-pane -t 1
#tmux resize-pane -Z -t 1
