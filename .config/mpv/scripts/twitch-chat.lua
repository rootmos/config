local msg = require "mp.msg"
local utils = require "mp.utils"

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
    local start = os.date("!%FT%T+00:00")
    local start_offset = -1 * mp.get_property("time-pos")
    msg.info(("offsetting comments with respect to: %s (wall clock) %s (content offset)"):format(start, start_offset))

    local done = false
    function go()
        mp.commandv("sub-reload")
        if not done then
            mp.add_timeout(mp.get_property("time-remaining")/2, go)
        end
    end

    local timer = nil
    timer = mp.add_periodic_timer(5.0, function()
        if utils.file_info(srt) then
            if mp.commandv("sub-add", srt) then
                timer:kill()
                local t = mp.get_property("time-remaining")
                if t then
                    mp.add_timeout(math.min(t/2, 30.0), go)
                else
                    mp.add_timeout(5.0, go)
                end
            end
        end
    end)

    mp.command_native_async({
            name = "subprocess",
            args = {"twitch-cli", "--log=info", "comments", "--start", start, "--offset", tostring(start_offset), "--output", srt, content},
            playback_only = false
        },
        function(success, result, err)
            if success then
                mp.commandv("sub-reload")
            else
                msg.error(("success=%s result=%s err=%s"):format(success, result, err))
            end
            done = true
        end
    )

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
        mp.add_timeout(20, launch_twitch_chat)
        mp.add_key_binding("t", "twitch_chat_tmux", launch_twitch_chat_tmux)
    end
end)
