-- original slicing.lua script by Kagami Hiiragi:
-- https://github.com/Kagami/mpv_slicing/blob/master/slicing.lua

local msg = require "mp.msg"
local utils = require "mp.utils"

local state = {
    from = nil,
    to = nil,
    video = true
}

timestamp = function (d) return string.format("%02d:%02d:%02.03f", d/3600, d%3600/60, d%60) end
osd = function (str) mp.osd_message(str, 3) end

function cut(from, to)
    local i = mp.get_property("stream-path")
    local b = utils.join_path(os.getenv("HOME"), "slices")
    local o = utils.join_path(b,
        string.format("%s.%s-%s", mp.get_property("filename"),
            timestamp(from), timestamp(to)))

    os.execute(string.format([[mkdir -p "%s"]], b))
    local cmd = nil
    if state.video then
        cmd = string.format(
            [[ffmpeg -loglevel warning -accurate_seek -ss "%s" -to "%s" -i "%s" -c copy -f matroska "%s"]],
            timestamp(from), timestamp(to), i, o)
    else
        cmd = string.format(
            [[ffmpeg -loglevel warning -accurate_seek -ss "%s" -to "%s" -i "%s" -vn -c copy -f matroska "%s"]],
            timestamp(from), timestamp(to), i, o)
    end
    msg.info(cmd)
    os.execute(cmd)
end

function toggle_mark()
    local p = mp.get_property_number("time-pos")
    if state.from then
        if p < state.from then
            state.from, state.to = p, state.from
        else
            state.to = p
        end
        osd(string.format("Cut fragment: %s - %s",
                timestamp(state.from),
                timestamp(state.to)))
        cut(state.from, state.to)
        state.from, state.to = nil, nil
    else
        state.from = p
        osd(string.format("Marked %s as start position", timestamp(p)))
    end
end

function toggle_audio()
    state.video = not state.video
    osd("Video capturig is " .. (state.video and "enabled" or "disabled"))
end

function clear_mark()
    state.from = nil
    osd("Cleared mark")
end

mp.add_key_binding("c", "slicing_mark", toggle_mark)
mp.add_key_binding("C", "slicing_clear_mark", clear_mark)
mp.add_key_binding("a", "slicing_audio", toggle_audio)
