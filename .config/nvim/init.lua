vim.opt.modeline = false
vim.opt.ruler = true
vim.opt.number = true
vim.opt.showcmd = true
vim.opt.wrap = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.showmatch = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.colorcolumn = "+1"

vim.opt.list = true
vim.opt.listchars = "tab:» "

vim.api.nvim_set_keymap("i", "hh", "<Esc>", { noremap = true })
vim.api.nvim_set_keymap("i", "uu", "<Esc>:w<CR>", { noremap = true })
vim.api.nvim_set_keymap("i", "ii", "<Esc>:wall<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "q", "<NOP>", { noremap = true })
vim.api.nvim_set_keymap("n", "Q", "<NOP>", { noremap = true })

vim.g.mapleader = ","
vim.g.maplocalleader = '-'

require('plugins')

vim.colorscheme = "solarized"
vim.api.nvim_set_var("airline_solarized_bg", "dark")
vim.api.nvim_set_var("airline_theme", "solarized")

require('nvim-treesitter.configs').setup {
    --highlight = {
        --enable = true,
        --additional_vim_regex_highlighting = false,
    --},
    --indent = {
        --enable = true,
    --},
    ensure_installed = {
       'c', 'lua', 'bash'
    },
}

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
require('tabs-vs-spaces.config').default = 4

vim.g.do_filetype_lua = 1
vim.filetype.add {
    filename = { [".k"] = "sh" },
}
