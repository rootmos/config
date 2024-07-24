local M = {}

local path = os.getenv("HOME") .. "/.config/displayswitcheroo/.toggle"

function M.state()
    local f = io.open(path, "r")
    local st
    if f ~= nil then
        st = tonumber(f:read())
        f:close()
    else
        st = 0
    end

    return st
end

function M.flip()
    local st = M.state()

    if st == 0 then
        st = 1
    else
        st = 0
    end
    
    local f = io.open(path, "w")
    f:write(st)
    f:close()

    return st
end

return M
