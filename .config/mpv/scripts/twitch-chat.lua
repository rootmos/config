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

function launch_twitch_chat_vod()
    local vid = mp.get_property("filename")
    local srt = os.tmpname()
    msg.info(("fetching comments for video: %s"):format(vid))
    msg.debug(("saving comments to file: %s"):format(srt))
    mp.command_native_async({
            name = "subprocess",
            args = {"twitch-cli", "comments", "--output", srt, vid},
            playback_only = false
        },
        function(success, result, err)
            if success then
                mp.commandv("sub-add", srt)
                set_chat_sub_style()
            else
                msg.error(("success=%s result=%s err=%s"):format(success, result, err))
            end
        end
    )
end

function set_chat_sub_style()
    mp.set_property("sub-align-x", "left")
    mp.set_property("sub-font-size", 25)
    mp.set_property("sub-shadow-offset", 2)
    mp.set_property("sub-back-color", "0/0/0/0.7")
end

mp.add_hook("on_load", 50, function()
    local p = mp.get_property("path")
    if p:find("https://twitch.tv") or p:find("https://www.twitch.tv") then
        mp.add_key_binding("t", "twitch_chat_live", launch_twitch_chat)
        mp.add_key_binding("T", "twitch_chat_vod", launch_twitch_chat_vod)
    end
end)
