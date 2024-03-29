" Plugins
call plug#begin()
Plug 'preservim/nerdtree'

Plug 'elixir-editors/vim-elixir'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'arrufat/vala.vim'
Plug 'mattn/emmet-vim'

Plug 'skurob/robpur-vim'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'

Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp' 
Plug 'hrsh7th/cmp-buffer'

Plug 'saadparwaiz1/cmp_luasnip' 
Plug 'L3MON4D3/LuaSnip' 
" Plug 'wlangstroth/vim-racket'
call plug#end()

" Basic settings.
set number
set rnu
set clipboard=unnamedplus
syntax on
set noswapfile
set backspace=indent,eol,start
"set guifont=Terminus:h10
set mouse=a
set signcolumn=no

" whitespaces
set expandtab
set tabstop=4
set softtabstop=4
set autoindent

set shiftwidth=4
set wildmenu

" map hotkeys
" map  <C-l> :tabn<CR>
" map  <C-h> :tabp<CR>
" map  <C-n> :tabnew<CR>

" colorscheme settings.
colorscheme robpur
set cursorline
highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212


" NERDTree configuration.
nmap <C-f> :NERDTreeToggle<CR>
let NERDTreeWinSize = 50 

" No auto gofmt on save
" let g:go_fmt_autosave=0
" let g:go_asmfmt_autosave=0
"

lua << END
require('lualine').setup()
require('mason').setup()
require("mason-lspconfig").setup()

local capabilities = require("cmp_nvim_lsp").default_capabilities()

local lspconfig = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
local servers = { 'gopls', 'pylsp', 'emmet_language_server', 'tsserver', 'hls' }
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    -- on_attach = my_custom_on_attach,
    capabilities = capabilities,
  }
end

-- luasnip setup
local luasnip = require 'luasnip'

local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-u>'] = cmp.mapping.scroll_docs(-4), -- Up
    ['<C-d>'] = cmp.mapping.scroll_docs(4), -- Down
    -- C-b (back) C-f (forward) for snippet placeholder navigation.
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'buffer' },
    { name = 'path' },
  },
}

local lspconfig = require("lspconfig")

lspconfig.gopls.setup({
  settings = {
    gopls = {
      analyses = {
        unusedparams = true,
      },
      staticcheck = true,
    },
  },
})

lspconfig.pylsp.setup({
  settings = {
    -- configure plugins in pylsp
    pylsp = {
      plugins = {
        pyflakes = {
            enabled = false
            },
        pycodestyle = {
            enabled = false
            }, 
        -- pylint = {enabled = false},
      },
    },
  },
})

lspconfig.emmet_language_server.setup({
  init_options = {
    excludeLanguages = {},
    extensionsPath = {},
    preferences = {},
    showAbbreviationSuggestions = true,
    showExpandedAbbreviation = "always",
    showSuggestionsAsSnippets = false,
    syntaxProfiles = {},
    variables = {},
  },
})

END

nnoremap <C-n> <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <C-d> <cmd>lua vim.lsp.buf.hover()<cr>

" ry for turn on rnu, rn for turn off.
nnoremap <leader>ry :set rnu<CR>
nnoremap <leader>rn :set nornu<CR>

