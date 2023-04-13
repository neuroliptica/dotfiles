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
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
call plug#end()

" Basic settings.
set number
set rnu
set clipboard=unnamedplus
syntax on
"filetype plugin indent on
set backspace=indent,eol,start
set guifont=Terminus:h10
set mouse=a
set t_Co=256

" whitespaces
set expandtab
set tabstop=4
set softtabstop=4
set autoindent

"set textwidth=80
set shiftwidth=4
set wildmenu

" map hotkeys
map  <C-l> :tabn<CR>
map  <C-h> :tabp<CR>
map  <C-n> :tabnew<CR>

" colorscheme settings.
colorscheme robpur
set cursorline
highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212

" block cursor.
let &t_SI = "\<esc>[5 q"  " blinking I-beam in insert mode
let &t_SR = "\<esc>[3 q"  " blinking underline in replace mode
let &t_EI = "\<esc>[ q"  " default cursor (usually blinking block) otherwiseo


" NERDTree configuration.
" ctrl + f open
nmap <C-f> :NERDTreeToggle<CR>
" auto exit when nerdtree is only window opened.  
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeWinSize = 50 

" No auto gofmt on save
" let g:go_fmt_autosave=0
" let g:go_asmfmt_autosave=0
"

lua << END
require('lualine').setup()
END

nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>

" ry for turn on rnu, rn for turn off.
nnoremap <leader>ry :set rnu<CR>
nnoremap <leader>rn :set nornu<CR>

