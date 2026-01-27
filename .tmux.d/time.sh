RUTIN="$HOME/git/rutin"
tmux move-window -t 0
tmux new-window -n "klocka" -c "$RUTIN/klocka"
tmux split-window -d -v -c "$RUTIN/progress"
tmux split-window -d -h -c "$RUTIN/be-on-time" -t 1

tmux send-keys -t 1 "C-l" "klocka TDFJQ"
tmux send-keys -t 2 "C-l" "./be-on-time "
tmux send-keys -t 3 "C-l" "progress log" Enter "progress add "

tmux select-pane -t 1
tmux resize-pane -Z -t 1
