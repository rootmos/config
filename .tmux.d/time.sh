RUTIN="$HOME/git/rutin"
tmux move-window -t 0
tmux new-window -n "klocka" -c "$RUTIN/klocka"
tmux split-window -d -h -c "$RUTIN/progress"
tmux split-window -d -v -c "$RUTIN/be-on-time" -t 2 -b

tmux send-keys -t 1 "C-l" "klocka TDFJQ"
tmux send-keys -t 2 "C-l" "./be-on-time "
tmux send-keys -t 3 "C-l" "progress add "
