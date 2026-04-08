export L="$HOME/L"

tmux move-window -t 0
tmux new-window -d -n "lit" -c "$L/lit"
tmux new-window -d -n "man" -c "$L/man"

export COURSE="oau376"
tmux new-window -n "$COURSE" -c "$L/$COURSE"
tmux split-window -v -c "$L/$COURSE"
tmux send-keys -t 1 "C-l" "c make" Enter
tmux send-keys -t 2 "C-l" "h" Enter

ANTECKNINGAR="$L/anteckningar"
tmux new-window -n "$(basename "$ANTECKNINGAR")" -c "$ANTECKNINGAR"
tmux split-window -v -c "$ANTECKNINGAR"
tmux send-keys -t 1 "C-l" "c make" Enter
tmux send-keys -t 2 "C-l" "h" Enter
