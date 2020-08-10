local msg = require "mp.msg"
local utils = require "mp.utils"

local start = nil

function launch_twitch_chat_tmux()
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

function launch_twitch_chat()
    local content = mp.get_property("filename")
    local srt = os.tmpname()
    msg.info(("fetching comments for content: %s"):format(content))
    msg.debug(("saving comments to file: %s"):format(srt))
    msg.debug(("offsetting comments with respect to: %s"):format(start))

    mp.command_native_async({
            name = "subprocess",
            args = {"twitch-cli", "comments", "--start", start, "--output", srt, content},
            playback_only = false
        },
        function(success, result, err)
            if not success then
                msg.error(("success=%s result=%s err=%s"):format(success, result, err))
            end
        end
    )

    function go()
        mp.commandv("sub-reload")
        mp.add_timeout(mp.get_property("time-remaining")/2, go)
    end

    local timer = nil
    timer = mp.add_periodic_timer(5.0, function()
        if utils.file_info(srt) then
            if mp.commandv("sub-add", srt) then
                timer:kill()
                mp.add_timeout(mp.get_property("time-remaining")/2, go)
            end
        end
    end)

    set_chat_sub_style()
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
        start = os.date("!%FT%T+00:00")
        launch_twitch_chat()
        mp.add_key_binding("t", "twitch_chat_tmux", launch_twitch_chat_tmux)
    end
end)
