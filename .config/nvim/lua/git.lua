local M = {
    _git = nil,
}

function M.git()
    if M._git == nil then
        M._git = io.popen("which git 2>/dev/null"):read("l")
        if M._git == nil then
            M._git = false
        end
    end
    return M._git
end

function M.toplevel(path)
    return io.popen(string.format("env -C '%s' '%s' rev-parse --show-toplevel 2>/dev/null", path or ".", M.git())):read("l")
end

return M
