

" VUNDLE SETUP
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
" Plugin 'tpope/vim-fugitive'

" plugin from http://vim-scripts.org/vim/scripts.html
"Plugin 'genutils'
"Plugin 'multiselect'

Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-syntax'
" ** Below is already in 7.4 as gn/gN
" Plugin 'kana/vim-textobj-lastpat'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-smartword'
Plugin 'kana/vim-smartinput'
Plugin 'kana/vim-niceblock'
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge  <Plug>(smartword-ge)



"https://github.com/terryma/vim-multiple-cursors#installation
Plugin 'terryma/vim-expand-region'
Plugin 'terryma/vim-multiple-cursors'
" Default mapping
let g:multi_cursor_next_key='<C-d>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

" Plugin 'scrooloose/nerdcommenter'
Plugin 'tomtom/tcomment_vim'
" Plugin 'Valloric/YouCompleteMe'





" from https://github.com/grigio/vim-sublime/blob/master/vimrc
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
"Plugin 'kien/ctrlp.vim'



Plugin 'ervandew/supertab'

Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

Plugin 'ternjs/tern_for_vim'


Plugin 'vim-airline/vim-airline'
Plugin 'airblade/vim-gitgutter'

Plugin 'scrooloose/nerdtree'

" Plugin 'Raimondi/delimitMate'

Plugin 'Valloric/MatchTagAlways'

Plugin 'pangloss/vim-javascript'
"***** regexpengine=1 will cause problem
" set regexpengine=1
" syntax enable

Plugin 'JavaScript-Indent'

" Search for selected text
Plugin 'thinca/vim-visualstar'


" Auto close (X)HTML tags
Plugin 'alvan/vim-closetag'
" Quick (X)HTML tag motion
Plugin 'gcmt/breeze.vim'

" Plugin auto-pares
" https://github.com/jiangmiao/auto-pairs

" Color Themes
Plugin 'flazz/vim-colorschemes'
"colorscheme Monokai

Plugin 'guns/xterm-color-table.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


set ts=2
set sts=2
set sw=2
set expandtab
set history=10000
set laststatus=2
set number

" set mouse=a

" allow unsaved background buffers and remember marks/undo for them
set hidden
set showmatch

set wildmenu
set wildmode=longest:list,full


" display incomplete commands
set showcmd

" Switch paste mode
set pastetoggle=<F2>

" Insert mode helper key
" Map C-K, C-J to Up and Down key
" let VI move cursor as Emacs
inoremap <C-k> <C-R>=pumvisible() ? "\<lt>C-P>" : "\<lt>Up>"<CR>
inoremap <C-j> <C-R>=pumvisible() ? "\<lt>C-N>" : "\<lt>Down>"<CR>
imap <C-p> <C-k>
imap <C-n> <C-j>
imap <C-f> <Right>
imap <C-b> <Left>

imap <CR> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>CR>"<CR>
imap <C-I> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>Tab>"<CR>
" imap {} {<C-G>u<CR><CR><Up><C-I>




" inoremap <C-H> <C-R>="\<lt>Left>"<CR>
" inoremap <C-L> <C-R>="\<lt>Right>"<CR>
" map // <Leader>c | "Map C-/ not work, instead map to C-_, it's same

" Duplicate line
" ** MUST ** .zshrc add line
" stty -ixon
" to prevent Ctrl-S act as ScrollLock
nmap <c-s>d YP
" Switch tab
nmap tt gt

" Using pppp continuesly
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Move line up/down
nmap <C-J> dd]pk<CR>
nmap <C-K> ddk]Pk<CR>

function! GetCurChar()
  return getline('.')[col('.')-1]
endfunction


function! g:M5IndentBrackets()
  if match(")]}", GetCurChar())>-1
    normal %
  endif
  if match("([{", GetCurChar())>-1
    if col('.') == col('$')-1
      return
    endif
  endif
  let pos = searchpairpos('[(\[{]', '', '[\)\]\}]', 'bcW') 
  let action = "%i\<CR>\<C-O>%\<C-O>a\<CR>"
  call feedkeys(action)
endfunction

map ;' :call g:M5IndentBrackets()<cr>
imap ;' <Esc>:call g:M5IndentBrackets()<cr>
inoremap ;; <c-o>:call feedkeys("\<c-o>a")<cr>


map <Leader>k :NERDTreeToggle<CR>


" jump to all visible opening tags after the cursor position
"nmap <leader>j <Plug>(breeze-jump-tag-forward)
 ""jump to all visible opening tags before the cursor position
"nmap <leader>J <Plug>(breeze-jump-tag-backward)

"" jump to all visible HTML attributes after the cursor position
"nmap <leader>a <Plug>(breeze-jump-attribute-forward)
"" jump to all visible HTML attributes before the cursor position
"nmap <leader>A <Plug>(breeze-jump-attribute-backward)

" move to the next tag
nmap <leader>n <Plug>(breeze-next-tag)
" move to the previous tag
nmap <leader>b <Plug>(breeze-prev-tag)

" move to the next attribute
nmap <leader>N <Plug>(breeze-next-attribute)
" move to the previous attribute
nmap <leader>B <Plug>(breeze-prev-attribute)


" http://vimcasts.org/episodes/show-invisibles/
" http://stackoverflow.com/questions/1675688/make-vim-show-all-white-spaces-as-a-character
" Shortcut to rapidly toggle `set list`
nmap ,l :set list!<CR>
" set listchars = eol: $, tab: >-, trail: ~, extends: >, precedes:<
set list listchars=tab:\|\  
" highlight Whitespace cterm=underline gui=underline ctermbg=NONE guibg=NONE ctermfg=yellow guifg=yellow
" autocmd ColorScheme * highlight Whitespace gui=underline ctermbg=NONE guibg=NONE ctermfg=yellow guifg=yellow
" match Whitespace / \+/

"Invisible character colors 
"highlight NonText guifg=#fa4a59
"highlight SpecialKey guifg=#fa4a59


if &term =~ "xterm"
	let &t_SI = "\<Esc>]12;purple\x7"
	let &t_EI = "\<Esc>]12;blue\x7"
endif

" For MacVim in iTerm2
" let &t_SI = "\<Esc>]50;CursorShape=1\x7"
" let &t_EI = "\<Esc>]50;CursorShape=0\x7"


" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
set cursorline
autocmd InsertEnter * set nocursorline "| highlight CursorLine ctermbg=233
autocmd InsertLeave * set cursorline "| highlight CursorLine ctermbg=16


