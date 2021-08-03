local msg = require "mp.msg"

function add_to_list()
    local p = mp.get_property("path")
    msg.info(("listing: %s"):format(p))
    local fn = mp.get_property("filename")
    local cmd = string.format("l \"$(l | dmenu -p \"%s\")\" + \"%s\"", fn, p)
    os.execute(cmd)
end

mp.add_key_binding("F1", "add-to-list", add_to_list)
