" PerseoGI init.vim
"runtime! macros/dvorak.vim

" VIM general settings ------------------------------------------------------{{{
" + Interface ---------------------------------------------------------------{{{
syntax on
set noshowmatch
set hidden
set noerrorbells                " Stop sound effects
set number                      " Line numbers
set relativenumber
set nowrap                      " If a line goes our of screen it will not go line below
" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=50
" + }}}

" + Text editor -------------------------------------------------------------{{{
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab                   " Convert tab character to spaces
set smartindent                 " Best job to indent for you
" + }}}

" + Search ------------------------------------------------------------------{{{
set smartcase                   " Case sensitive searching
set nohlsearch
set incsearch                   " By the time start searching it highlight results before press enter
" + }}}
" + File manipulation and versioning ----------------------------------------{{{
set noswapfile                  " Dont create vim.swp files
set nobackup                    " Dont make a backup
set undodir=~/.vim/undodir      " Store a backup in undo directory
set undofile                    " Store modified file per file in undo directory
" + }}}

" + GUI settings ------------------------------------------------------------{{{
set scrolloff=8
set guicursor=
set termguicolors
set noshowmode                  " Dont show Vim mode status => replaced by vim-airline
" Give more space for displaying messages.
set cmdheight=2
" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
" Column mark
set colorcolumn=100
highlight ColorColumn ctermbg=0 guibg=lightgrey
" + }}}


" }}}

" VIM Pluggins {{{
call plug#begin('~/.vim/plugged')
Plug 'gruvbox-community/gruvbox'        " Colorscheme
Plug 'sainnhe/gruvbox-material'
"Plug 'vim-utils/vim-man'                " Man pages
Plug 'tpope/vim-fugitive'               " Git blames, logs...

" To avoid file not found errors on C/C++, compile_commands.json must be
" created on the path. Use $ bear make ... to generate automatically
Plug 'neoclide/coc.nvim', {'branch': 'release'}     " Completer
" To enable configure coc-settings.json with
"       clangd.semanticHighlighting": true
Plug 'jackguo380/vim-lsp-cxx-highlight' " Syntax highlighter for C/C++ to be used with Coc [semanticHighlighting]

Plug 'tweekmonster/gofmt.vim'
Plug 'mbbill/undotree'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'ThePrimeagen/vim-be-good', {'do': './install.sh'}
Plug 'zhou13/vim-easyescape'
"Plug 'prabirshrestha/async.vim'
"Plug 'prabirshrestha/vim-lsp'
"Plug 'prabirshrestha/asyncomplete.vim'
"Plug 'prabirshrestha/asyncomplete-lsp.vim'
"Plug 'prabirshrestha/asyncomplete-file.vim'
Plug 'preservim/nerdcommenter'          " Comments
Plug 'kshenoy/vim-signature'            " Show marks
Plug 'vuciv/vim-bujo'                   " Minimalist TODO list management
" Vim Airline: botton bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'justinmk/vim-sneak'               " Vim search word given two letters  TODO pensar si cambiarlo a f
Plug 'jiangmiao/auto-pairs'             " Auto open close pairs, best plug of Augost-2020
Plug 'soywod/iris.vim'                  " Email client
"Plug 'mcchrish/nnn.vim'
"Plug 'floobits/floobits-neovim'         " Colaborate Vim ~= VSCode Live Share pluggin or Atom Teletype
" After having tested coc-html, coc-emmet, coc-snippets, this is the best option for auto-closing html tags on jsx
Plug 'alvan/vim-closetag'
Plug 'ryanoasis/vim-devicons'           " Icons for coc-explorer
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh' }
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
call plug#end()

" + Vim pluggins settings ---------------------------------------------------{{{

" ++ Theme customization ----------------------------------------------------{{{

" +++ Vim Airline settings --------------------------------------------------{{{

" Set up powerline fonts to enable cute, non rectangle interface
let g:airline_powerline_fonts = 1
" Show current open buffers as tabs on the top
let g:airline#extensions#tabline#enabled = 1
" Show only last (tail) name of the file on tabs
let g:airline#extensions#tabline#formatter = 'unique_tail'
" Set theme with gruvbox
let g:airline_theme='gruvbox'
" +++ }}}

" +++ Gruvbox settgings -----------------------------------------------------{{{

let g:gruvbox_contrast_dark = 'hard'
if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
" Disable invert selection, you are wellcome eyes!
let g:gruvbox_invert_selection='0'
" +++ }}}

" remove the file encoding with Github account
let g:airline_section_y='@PerseoGI'
" remove separators for empty sections
"let g:airline_skip_empty_sections = 1

" Leave colorcheme config here, after airline config
colorscheme gruvbox
set background=dark

" ++ }}}


" ++ CoC  config -------------------------------------------------------{{{
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()

command! -nargs=0 Prettier :CocCommand prettier.formatFile
" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')
" ++ }}}

" ++ VimBeGood config -------------------------------------------------------{{{
let g:vim_be_good_floating = 0
" ++ }}}

" ++ vim go (polyglot) setting ----------------------------------------------{{{
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_highlight_function_parameters = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_generate_tags = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_variable_declarations = 1
let g:go_auto_sameids = 1
" ++ }}}

" ++ RipGrep settings -------------------------------------------------------{{{
" Allow RipGrep (FZF) to detect the route, search for git repo and optimize searchs
if executable('rg')
    let g:rg_derive_root='true'
endif
let loaded_matchparen = 1
" ++ }}}

" ++ Browser tree configuration ---------------------------------------------{{{
let loaded_netrwPlugin = 1  " Disable netrw on open
let g:netrw_browse_split = 2
let g:vrfr_rg = 'true'
let g:netrw_banner = 0  " Disable help info on browser tree
let g:netrw_winsize = 25
"" ++ }}}

" ++ vim-easyescape ---------------------------------------------------------{{{
" Escape to normal mode pressing 'jk' or 'kj' at the same time
let g:easyescape_chars = { "j": 1, "k": 1 }
let g:easyescape_timeout = 100
"++ }}}

" ++ ToDo bujo settings -----------------------------------------------------{{{
let g:bujo#window_width = 70
"++ }}}


" Iris settings -------------------------------------------------------------{{{
let g:iris_name = "Perseo"
"let g:iris_mail = "perseo.gi98@gmail.com"
"let g:iris_imap_host  = "imap.gmail.com"
"let g:iris_imap_port  = 993
"let g:iris_smtp_host  = "smtp.gmail.com"
"let g:iris_smtp_port  = 587
"let g:iris_imap_passwd_filepath = "/home/perseo/.config/nvim/gmail_password_iris.gpg"
"let g:iris_smtp_passwd_filepath = "/home/perseo/.config/nvim/gmail_password_iris.gpg""

let g:iris_mail = "alejandro-perseo.gutierrez.izquierdo@alumnos.upm.es"
let g:iris_imap_host  = "correo.alumnos.upm.es"
let g:iris_imap_port  = 993
let g:iris_imap_login = "alejandro-perseo.gutierrez.izquierdo"
let g:iris_smtp_host  = "smtp.upm.es"
let g:iris_smtp_port  = 587
let g:iris_emails_chunk_size = 50
let g:iris_imap_passwd_filepath = "/home/perseo/.config/nvim/upm_password_iris.gpg"
let g:iris_smtp_passwd_filepath = "/home/perseo/.config/nvim/upm_password_iris.gpg""

"++ }}}

" nnn settings  -------------------------------------------------------------{{{
"let g:nnn#layout = { 'left': '~30%' }
" Floating window (neovim latest and vim with patch 8.2.191)
let g:nnn#layout = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Debug' } }

autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NnnPicker | endif
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | NnnPicker| endif
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'CocCommand explorer' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
"autocmd vimenter * NnnPicker
"++ }}}

" + }}}
" }}}

" Mappings ------------------------------------------------------------------{{{

" Configure leader key to spacebar. Leader key allow enter custom user command mode
let mapleader = " "

" Reload vim.init file
nnoremap <Leader><CR> :so ~/.config/nvim/init.vim<CR>

" + Buffer maps -------------------------------------------------------------{{{
" Remap convenient change between windows commands
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

" Move among buffers with CTRL (enable g:airline#extensions#tabline#enabled to
" see open buffers)
map <C-L> :bnext<CR>
map <C-H> :bprev<CR>
" Close buffer
nnoremap <C-w>c :bd<CR>

" Buffer resizing
nnoremap <Leader>+ :vertical resize +5<CR>
nnoremap <Leader>= :vertical resize +5<CR>
nnoremap <Leader>- :vertical resize -5<CR>
nnoremap <Leader>rp :resize 100<CR>
" + }}}

" + Useful mappings ---------------------------------------------------------{{{

" Make Y as it should be!
nnoremap Y y$
" X as a i<backspace>
vnoremap X "_d

inoremap <C-c> <esc>

" Visual-Line move lines up or down
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Visual selection after shifting
vnoremap < <gv
vnoremap > >gv

" Surround with quotation marks
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

" Auto indent all file
nnoremap <Leader>i gg=G<C-o>

" Capitalize or lower case current word
inoremap <C-u> <esc>viw~ea

" Operator pending mappings
" Same as Xi(  shorted to Xp
onoremap p i(
onoremap l i{
onoremap b /return<cr>
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap il{ :<c-u>normal! F}vi{<cr>
" + }}}

" + Clipboard config --------------------------------------------------------{{{
set clipboard+=unnamedplus
" Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" Copy all file on clipboard
nnoremap <leader>ya gg"+yG<C-o>

" Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P
" + }}}


" + Bro mappings ------------------------------------------------------------{{{
" ... Keep away from dangerous arrows !
"noremap <left> :echo "Use H bro :("<cr><left>
"noremap <right> :echo "Use L bro :("<cr><right>
"noremap <up> :echo "Use K bro :("<cr><up>
"noremap <down> :echo "Use J bro :("<cr><down>
"noremap <esc> :echo "Don't even think of ESC. Use jk or kj :)"<cr><esc>
" + }}}

" + Vim Pluggings mappings --------------------------------------------------{{{

" ++ NERDCommenter mappings -------------------------------------------------{{{
" Comments map to <C-/> (vim register for '/' is  '_')
nmap <C-_>   <Plug>NERDCommenterToggle
" gv leave selected lines
vmap <C-_>   <Plug>NERDCommenterToggle<CR>gv
" ++ }}}

" ++ Undotree mappings ------------------------------------------------------{{{
" Open Undotree
nnoremap <leader>u :UndotreeShow<CR>
" ++ }}}

" ++ Find mappings ----------------------------------------------------------{{{
" Find current word in the project using rg
nnoremap <leader>pw :Rg <C-R>=expand("<cword>")<CR><CR>
" Find a word on project using rg
nnoremap <Leader>ps :Rg<SPACE>
" Find and refactor current word in the project
nnoremap <leader>prw :CocSearch <C-R>=expand("<cword>")<CR><CR>
" Open vim help for current word
nnoremap <leader>vhw :h <C-R>=expand("<cword>")<CR><CR>
" ++ }}}}

" ++ File explorer mappings -------------------------------------------------{{{
" GFiles only works with git repo and will only display added files on .git
nnoremap <C-p> :GFiles<CR>
" Files is the best alternative for GFiles when there is no git initialized
nnoremap <Leader>pf :Files<CR>
" Open classical Ex-plorer on the left
nnoremap <leader>pv :CocCommand explorer --quit-on-open<CR>
"nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>

" <leader>n  to open nnn explorer => great sustitute of Ex

" ++ }}}

" ++ Coc mappings: GoTo code navigation --------------------------------------{{{
nmap <leader>gd <Plug>(coc-definition)
nmap <leader>gy <Plug>(coc-type-definition)
nmap <leader>gi <Plug>(coc-implementation)
nmap <leader>gr <Plug>(coc-references)
nmap <leader>rr <Plug>(coc-rename)
nmap <leader>[ <Plug>(coc-diagnostic-prev)
nmap <leader>] <Plug>(coc-diagnostic-next)
nmap <silent> <leader>gp <Plug>(coc-diagnostic-prev-error)
nmap <silent> <leader>gn <Plug>(coc-diagnostic-next-error)
nnoremap <leader>cr :CocRestart

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <C-space> coc#refresh()


" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Go to header (C/C++)
noremap <leader>gth :CocCommand clangd.switchSourceHeader<cr>

" ++ }}}

" ++ FuGITive mappings ------------------------------------------------------{{{
nmap <leader>gh :diffget //3<CR>
nmap <leader>gu :diffget //2<CR>
nmap <leader>gs :G<CR>
" ++ }}}

" ToDo Bujo mappings --------------------------------------------------------{{{
nmap <C-S> <Plug>BujoAddnormal
imap <C-S> <Plug>BujoAddinsert
nmap <C-Q> <Plug>BujoChecknormal
imap <C-Q> <Plug>BujoCheckinsert
" ++ }}}


" Vim-sneak mappings --------------------------------------------------------{{{
"map t <Plug>Sneak_f
"map T <Plug>Sneak_F
" ++ }}}

" Iris mappings --------------------------------------------------------{{{
" ++ }}}

" Close tags mappings --------------------------------------------------------{{{
let g:closetag_filenames = '*.html,*.xhtml,*.js,*.ts,*.jsx,*.tsx'
" ++ }}}

" + }}}
" }}}

" Automatic commands --------------------------------------------------------{{{

" Remember last position in buffer
autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Tab size for JS and TS files
augroup TabSize
    autocmd!
    autocmd FileType javascript,typescript set tabstop=2 softtabstop=2 shiftwidth=2
augroup END

" Configure wrap mode only on Markdown files
augroup Markdown
    autocmd!
    autocmd FileType markdown set wrap
augroup END

" This is not working properly... should highlight yanked lines
augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END

autocmd BufWritePre * :call TrimWhitespace()

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun


" Visual Line macros: apply a macro to multiple lines by:
"   1. Recording a macro
"   2. Selecting multiple lines with V-Line mode
"   3. Applying macro to those lines
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>
function! ExecuteMacroOverVisualRange()
    echo "@".getcmdline()
    execute ":'<,'>normal @".nr2char(getchar())
endfunction

" }}}

" Custom abbreviations ------------------------------------------------------{{{
iabbrev clog console.log(
iabbrev @@ perseo.gi98@gmail.com
iabbrev af () => {}<left><CR><Tab>
" }}}

" Deprecated stuff ----------------------------------------------------------{{{
" Vim-man
"map <C-k> <Plug>(Man)
" }}}


" Experimental stuff

" vim:foldmethod=marker:foldlevel=4
