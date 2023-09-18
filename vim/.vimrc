"
" 自定义 VIM 基础配置
"
" 设置编码
set encoding=utf-8
set termencoding=utf-8
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
" 显示行号
set number
" 括号匹配
set showmatch
" 自动缩进
set autoindent
" 显示光标当前位置
set ruler
" tab宽度
set tabstop=4
" 自动缩进长度
set shiftwidth=4
" 显示空格与tab
set listchars=tab:>-,trail:-
" 显示状态栏
set laststatus=2
" 显示当前行行号，其他行显示相对行号
set relativenumber
" 垂直滚动时，当前行相对于底部的距离
set scrolloff=5
" 搜索时，高亮匹配
set hlsearch
" 输入搜索模式，每次自动匹配结果
"set incsearch
" 需要记录的VIM历史操作个数
set history=500
" 打开文件监视，防止外部篡改
set autoread
" 侦测文件类型
filetype on
" 语法高亮显示
syntax on
" 显示括号配对情况
set magic
" 突出当前行
set cursorline

" 改变主题
colorscheme onedark 
let g:airline_theme='onedark'

