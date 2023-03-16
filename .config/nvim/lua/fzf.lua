local function root()
    return require("git").toplevel(vim.fn.expand("%:p:h"))
end

local function spec()
    return {
        options = "--color=16",
        dir = root(),
    }
end

vim.keymap.set("n", "<leader>f", function() vim.fn["fzf#vim#files"]("", spec()) end)
vim.keymap.set("n", "<leader>t", function() vim.fn["fzf#vim#gitfiles"]("", spec()) end)
vim.keymap.set("n", "<leader>a", function() vim.fn["fzf#vim#ag"]("", spec()) end)
vim.keymap.set("n", "<leader>b", function() vim.fn["fzf#vim#buffers"]("", spec()) end)
