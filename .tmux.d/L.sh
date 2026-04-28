export L="$HOME/L"

tmux move-window -t 0
tmux new-window -d -n "lit" -c "$L/lit"
tmux new-window -d -n "man" -c "$L/man"

project() {
    tmux new-window -n "$1" -c "$L/$1"
    tmux split-window -v -c "$L/$1"
    tmux send-keys -t 1 "C-l" "c make" Enter
    tmux send-keys -t 2 "C-l" "h" Enter
}

project oau278
project anteckningar
project oau376
