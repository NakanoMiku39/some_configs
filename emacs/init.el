;;; init.el
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of other files

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-packages)
(require 'theme)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; (require 'straight)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(setq confirm-kill-emacs #'yes-or-no-p)      ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t)                       ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq inhibit-startup-message t)             ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                 ; 关闭文件自动备份
(setq auto-save-default nil)                 ; 关闭自动保存文件
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(tool-bar-mode -1)                           ; （熟练后可选）关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条

(savehist-mode 1)                            ; （可选）打开 Buffer 历史记录保存
(setq display-line-numbers-type 'relative)   ; （可选）显示相对行号
(add-to-list 'default-frame-alist '(width . 80))  ; （可选）设定启动图形界面时的初始 Frame 宽度（字符数）
(add-to-list 'default-frame-alist '(height . 60)) ; （可选）设定启动图形界面时的初始 Frame 高度（字符数）

;; 更改现实字体大小 16pt
(set-face-attribute 'default nil :height 160)
;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; yaml改进换行
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(package-install 'orderless)
(setq competion-styles '(orderless))

;; 打开scratch buffer
(global-set-key (kbd "C-x s") 'scratch-buffer)

;; minibuffer action和自适应的context menu
(package-install 'embark)
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

;; 增强文件内搜索和跳转函数定义
(package-install 'consult)
;; replace swiper
(global-set-key (kbd "M-s") 'consult-line)
;; consult-imenu

(package-install 'embark-consult)
(package-install 'wgrep)
(setq wgrep-auto-save-buffer t)

(eval-after-load
    'consult
  '(eval-after-load
       'embark
     '(progn
        (require 'embark-consult)
        (add-hook
         'embark-collect-mode-hook
         #'consult-preview-at-point-mode))))

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;;使用ripgrep来进行搜索
;;consult-ripgrep

;;everyting
;;consult-locate
;; 配置搜索中文
(progn
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
  )
(eval-after-load 'consult
  (progn
    (setq
     consult-narrow-key "<"
     consult-line-numbers-widen t
     consult-async-min-input 2
     consult-async-refresh-delay  0.15
     consult-async-input-throttle 0.2
     consult-async-input-debounce 0.1)
    ))

(eval-when-compile
  (require 'use-package))

;; (use-package restart-emacs
;;   :ensure t)

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

;; 增强minibuffer补全
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; magit
(use-package magit
  :ensure t
  :defer 2)

;; 括号上色
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 增强minibuffer的annotaion
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; counsel
(use-package counsel
  :ensure t)

;; ivy的配置
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; 记录undo历史
(use-package undo-tree
  :ensure t
  :defer 2
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

;; mwin 光标移动
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; amx 记录命令历史
(use-package amx
  :ensure t
  :init (amx-mode))

(use-package smart-mode-line
  :ensure t
  :init (sml/setup))

(use-package which-key
  :ensure t
  :init (which-key-mode))
;; (load-theme 'dracula t)

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode))

(use-package company
  :hook (scala-mode . company-mode)
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next)
	      ("C-p" . 'company-select-previous))
  :ensure t
  :init (global-company-mode t)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq lsp-completion-provider :capf))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

;;modeline上显示我的所有的按键和执行的命令
(package-install 'keycast)
(add-to-list 'global-mode-string '("" keycast-mode-line))
(keycast-mode t)

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :ensure t

  :init
  (doom-modeline-mode t))

(use-package all-the-icons
  :if (display-graphic-p))

;; ;;; rust环境配置
;; (use-package rustic
;;   :ensure
;;   :bind (:map rustic-mod-map
;;       ("M-j" . lsp-ui-imenu)
;;       ("M-?" . lsp-find-references)
;;       ("C-c C-c l" . flycheck-list-errors)
;;       ("C-c C-c a" . lsp-execute-code-action)
;;       ("C-c C-c r" . lsp-rename)
;;       ("C-c C-c q" . lsp-wordspace-restart)
;;       ("C-c C-c Q" . lsp-workspace-shutdown)
;;       ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; 减少闪动可以取消这里的注释
;;   ;; (setq lsp-eldoc-hook nil)      
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; 注释下面这行可以禁用保存时 rustfmt 格式化
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; 所以运行 C-c C-c C-r 无需确认就可以工作，但不要尝试保存不是文件访问的 rust 缓存。
;;   ;; 一旦 https://github.com/brotzeit/rustic/issues/253 问题处理了
;;   ;; 就不需要这个配置了
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t)))


;; 手动安装rust-mode
;; (add-to-list 'load-path "~/rust-mode/")
;; (autoload 'rust-mode "rust-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(use-package lsp-mode
  :ensure
  :commands lsp
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :custom
  ;; 保存时使用什么进行检查，默认是 "check"，我更推荐 "clippy"
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Python
(use-package python
  :defer 5
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package pyvenv
  :ensure t
  :defer 5
  :config
  ;; (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  ;; (setq python-shell-interpreter "python3")  ; （可选）更改解释器名字
  (pyvenv-mode t)
  ;; （可选）如果希望启动后激活 miniconda 的 base 环境，就使用如下的 hook
  ;; :hook
  ;; (python-mode . (lambda () (pyvenv-workon "..")))
)

;; Pyright
(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
		  (require 'lsp-pyright)
		  (lsp-deferred))))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals)

;; (use-package eglot
;;   :config
;;   ;; 给 c-mode, c++-mode 配置使用 clangd-11 作为 LSP 后端
;;   ;; 需要主要的是，要根据上面你安装的 clangd 程序的名字填写这个配置
;;   ;; 我这里写成 clangd-11 是因为安装的 clangd 程序的名字为 clangd-11
;;   (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-19"))
;;   ;; (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-19"))
;;   ;; 使用 c-mode 时，开启 eglot
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   ;; 使用 c++-mode 时，开启 eglot
;;   (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
	("C-x t a"   . treemacs-add-project-to-workspace)
	("C-x t d"   . treemacs-remove-project-from-workspace))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(use-package vterm
    :ensure t)

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  :config
  (setq shackle-rules
	'((term-mode :regexp t
		     :select t
		     :size 0.3
		     :align t
		     :popup t
		     :quit t)
	  (vterm-mode :regexp t
		     :select t
		     :size 0.3
		     :align t
		     :popup t
		     :quit t)
	  (ansi-term :regexp t
		     :select t
		     :size 0.3
		     :align t
		     :popup t
		     :quit t))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages
   '(lsp-metals lsp-treemacs treemacs-projectile treemacs lsp-ui lsp-mode doom-modeline company-box company flycheck amx mwim undo-tree counsel doom-themes which-key vertico smart-mode-line orderless marginalia keycast embark consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
