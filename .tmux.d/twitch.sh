tmux move-window -t 0
tmux new-window -n "live+vods" "browse-twitch"
tmux new-window -n "journalctl" -d "journalctl --user -fu browse-twitch.service"
