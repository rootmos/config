local function root()
    return require("git").toplevel(vim.fn.expand("%:p:h"))
end

local function fzf_files()
    vim.fn["fzf#vim#files"](root())
end

local function fzf_buffers()
    vim.fn["fzf#vim#buffers"]()
end

local function fzf_ag()
    vim.fn["fzf#vim#ag"](root())
end

vim.keymap.set("n", "<leader>f", function() vim.fn["fzf#vim#files"](root()) end)
vim.keymap.set("n", "<leader>t", function() vim.fn["fzf#vim#gitfiles"](root()) end)
vim.keymap.set("n", "<leader>a", function() vim.fn["fzf#vim#ag"](root()) end)
vim.keymap.set("n", "<leader>b", function() vim.fn["fzf#vim#buffers"]() end)
