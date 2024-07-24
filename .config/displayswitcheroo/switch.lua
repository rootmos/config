local D = require("displayswitcheroo")()
local T = require("toggle")

local laptop = D["eDP-1"]
local desktop = D[0x7ad8cb74]
local livingroom = D[0xc0e5ff0c]
local abbebo = D[0x43b791a2]
local salen = D[0xde28a982]
local tv = D[0x5267d474]

local cooloff = nil

if desktop then
    if T.state() == 0 then
        D.left_right(desktop, laptop)
    else
        D.one(desktop)
    end
    cooloff = 10
elseif livingroom then
    D.one(livingroom)
elseif abbebo then
    if T.state() == 0 then
        D.left_right(abbebo, laptop)
    else
        D.one(abbebo)
    end
elseif salen then
    D.one(salen)
elseif tv then
    if T.state() == 0 then
        D.left_right(tv, laptop)
    else
        D.one(tv)
    end
else
    D.one(laptop)
    cooloff = 1
end

D.sync()

if cooloff then
    print(string.format("letting monitors settle: %ds...", cooloff))
    os.execute("sleep " .. cooloff)
    print("  ...and were (hopefully) stable")
end

os.execute("/home/gustav/.local/bin/statusbar reconfigure")
