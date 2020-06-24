local msg = require "mp.msg"

function launch_twitch_chat()
    local t = mp.get_property("filename")
    msg.verbose(("launching twitch chat: %s"):format(t))
    mp.command_native_async({
            name = "subprocess",
            args = {"env", "--default-signal", "twitch-cli-chat-tmux", t},
            playback_only = false
        },
        function(success, result, err)
            if not success then
                msg.error(("success=%s result=%s err=%s"):format(success, result, err))
            end
        end
    )
end

mp.add_hook("on_load", 50, function()
    local p = mp.get_property("path")
    if p:find("https://twitch.tv") then
        mp.add_key_binding("t", "twitch_chat", launch_twitch_chat)
    end
end)
