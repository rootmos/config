tmux move-window -t 0
tmux setenv TMP /tmp

tmux new-window
tmux send-keys "C-l" "tmp -n" Enter
