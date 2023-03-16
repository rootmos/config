return require('packer').startup(function()
    use 'wbthomason/packer.nvim'

    use 'altercation/vim-colors-solarized'

    use 'nvim-treesitter/nvim-treesitter'
    use 'nvim-treesitter/playground'

    use 'vim-airline/vim-airline'
    use 'vim-airline/vim-airline-themes'

    --use '~/git/tabs-vs-spaces'
    use 'rootmos/tabs-vs-spaces'

    --use 'https://git.sr.ht/~rootmos/tads.nvim'
    --use '~/git/tads.nvim'

    use 'neovimhaskell/haskell-vim'

    use 'https://github.com/hrsh7th/nvim-cmp'
    use 'https://github.com/hrsh7th/cmp-buffer'
    use 'https://github.com/hrsh7th/cmp-path'

    use 'https://github.com/scrooloose/nerdcommenter'

    use 'https://github.com/junegunn/fzf'
    use 'https://github.com/junegunn/fzf.vim'
end)
