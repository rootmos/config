local D = require("displayswitcheroo")()

local laptop = D["eDP-1"]
local desktop = D[0x7ad8cb74]
local livingroom = D[0xc0e5ff0c]
local abbebo = D[0x43b791a2]
local uto = D[0x1978990f]
local salen = D[0xde28a982]

local cooloff = nil

if desktop then
    D.left_right(desktop, laptop)
    cooloff = 10
elseif livingroom then
    D.one(livingroom)
elseif abbebo then
    D.left_right(abbebo, laptop)
elseif uto then
    D.one(uto)
elseif salen then
    D.one(salen)
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
