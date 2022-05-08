return require('packer').startup(function()
    use 'wbthomason/packer.nvim'

    use 'altercation/vim-colors-solarized'

    use 'nvim-treesitter/nvim-treesitter'
    use 'nvim-treesitter/playground'

    use 'vim-airline/vim-airline'
    use 'vim-airline/vim-airline-themes'

    use 'rootmos/tabs-vs-spaces'

    --use 'https://git.sr.ht/~rootmos/tads.nvim'
    use '~/git/tads.nvim'
end)
