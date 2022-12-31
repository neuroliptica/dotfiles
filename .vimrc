" Plugins
call plug#begin()
Plug 'preservim/nerdtree'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'arrufat/vala.vim'
Plug 'mattn/emmet-vim'
call plug#end()

" Basic settings.
set number
set clipboard=unnamedplus
syntax on
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
colorscheme zellner
set wildmenu

" map hotkeys
map  <C-l> :tabn<CR>
map  <C-h> :tabp<CR>
map  <C-n> :tabnew<CR>

" 256_noir colorscheme settings.
"colorscheme 256_noir
"set cursorline
"highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212

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

