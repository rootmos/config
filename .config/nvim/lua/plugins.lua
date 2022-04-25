return require('packer').startup(function()
    use 'altercation/vim-colors-solarized'

    use 'nvim-treesitter/nvim-treesitter'
    use 'nvim-treesitter/playground'

    use 'vim-airline/vim-airline'
    use 'vim-airline/vim-airline-themes'
end)
