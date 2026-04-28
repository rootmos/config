tmux move-window -t 0
tmux new-window -n "live+vods" "browse-twitch"
tmux new-window -n "lists" -d -c "$HOME/lists/twitch"

tmux new-window -n "journalctl" "journalctl --user -fu browse-twitch.service"
tmux split-window -v
tmux send-keys "C-l" "systemctl --user restart browse-twitch.service"
