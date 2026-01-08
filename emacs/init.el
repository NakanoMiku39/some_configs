;;; init.el
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of other files

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(add-to-list 'load-path "~/.emacs.d/lisp")
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)
;; 安装use-package，包都通过use-package管理
(straight-use-package 'use-package)
;; (require 'init-packages)
(require 'straight)
(require 'theme)
(require 'yaml-mode)
(require 'org-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))



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
(add-to-list 'default-frame-alist '(width . 120))  ; （可选）设定启动图形界面时的初始 Frame 宽度（字符数）
;; (add-to-list 'default-frame-alist '(height . 80)) ; （可选）设定启动图形界面时的初始 Frame 高度（字符数）
;; (set-frame-size (selected-frame) 120 80)
;; ;; 更改显示字体大小 16pt
(set-face-attribute 'default nil :height 160)

(use-package project
  :straight t
  :config
  ;; 这一行是为了确保 project 确实被加载了，防止被后续的内置加载覆盖
  (require 'project))

;; 测量启动速度
(add-hook 'emacs-startup-hook
    (lambda ()
        (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                (float-time
                    (time-subtract after-init-time before-init-time)))
            gcs-done)))

;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; yaml改进换行
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

;; 打开scratch buffer
(global-set-key (kbd "C-x s") 'scratch-buffer)

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)) 

;; minibuffer action和自适应的context menu
(use-package embark
  :ensure t
  :bind
  ("C-;" . embark-act)
  :init
  (setq prefix-help-command 'embark-prefix-help-command))

;; 增强文件内搜索和跳转函数定义
(use-package consult
  :ensure t
  :bind
  ("M-s" . consult-line))

;; consult-imenu
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

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
  '(progn
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

(use-package compat
  :ensure t)

(use-package dirvish
  :ensure t
  :hook (after-init . dirvish-override-dired-mode)
  :bind(
	("C-x d" . dirvish)
	))

;; 快速行跳转
(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

;; 窗口切换
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

;; 增强minibuffer补全
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package posframe)

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
  :init (global-flycheck-mode)
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
  :straight nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :ensure t
  :config
  (add-to-list 'global-mode-string '("" keycast-mode-line " ")))

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :ensure t

  :init
  (doom-modeline-mode t))

(use-package all-the-icons
  :if (display-graphic-p))

;; ============================================================
;; LSP 客户端切换配置
;; ============================================================

;; 定义一个变量来决定使用哪个 LSP 客户端
;; 可选值: 'eglot 或 'lsp-mode
;; 修改这里的值并重启 Emacs (或者重新求值) 即可切换
(defconst my/lsp-client 'eglot)

;; ------------------------------------------------------------
;; 方案 A: Eglot (轻量级，Emacs 原生风格)
;; ------------------------------------------------------------
(when (eq my/lsp-client 'eglot)
  (use-package eglot
    :straight t
    :hook
    ;; 在这里统一管理所有语言的启动 Hook
    ((c-mode . eglot-ensure)
     (c++-mode . eglot-ensure)
     (rust-mode . eglot-ensure)
     (scala-mode . eglot-ensure)
     (python-mode . eglot-ensure))
    :config
    ;; 优化性能
    (setq eglot-events-buffer-size 0) ;; 关闭 event log 以提升性能
    (setq eglot-autoshutdown t)       ;; 关闭最后一个 buffer 时关闭 server

    ;; 配置 C/C++ 使用特定的 clangd 版本
    (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-19"))

    ;; Eglot 默认使用 Flymake。
    ;; 如果你开启了 global-flycheck-mode，为了避免冲突或重复显示，
    ;; 可以选择在 eglot 启动时禁用 flycheck，或者安装 flycheck-eglot。
    ;; 下面这个 hook 会在 eglot 启动时关闭当前 buffer 的 flycheck
    (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1)))
    ))

;; ------------------------------------------------------------
;; 方案 B: LSP-Mode (功能丰富，界面华丽)
;; ------------------------------------------------------------
(when (eq my/lsp-client 'lsp-mode)
  (use-package lsp-mode
    :straight t
    :commands lsp
    :hook
    (scala-mode . lsp)
    (rust-mode . lsp-deferred)
    (lsp-mode . lsp-lens-mode)
    :custom
    (lsp-eldoc-render-all t)
    (lsp-idle-delay 0.6)
    (lsp-completion-provider :capf) ;; 只有 lsp-mode 需要显式设置这个
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package lsp-ui
    :straight t
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil))

  ;; Scala 专用 (lsp-mode 独有)
  (use-package lsp-metals
    :straight t)

  ;; Treemacs 集成 (lsp-mode 独有)
  (use-package lsp-treemacs
    :straight t
    :after (treemacs lsp)))

;; ============================================================
;; 语言模式 (基础 Mode，无论用哪个 LSP 都需要)
;; ============================================================

;; Scala Mode
(use-package scala-mode
  :straight t
  :interpreter ("scala" . scala-mode))

;; SBT Mode
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; ============================================================
;; Treemacs (通用文件树，不依赖具体 LSP)
;; ============================================================
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

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(org-agenda-files '("~/org/TODO.org"))
 '(package-selected-packages
   '(lsp-metals lsp-treemacs treemacs-projectile treemacs lsp-ui lsp-mode doom-modeline company-box company flycheck amx mwim undo-tree counsel doom-themes which-key vertico smart-mode-line orderless marginalia keycast embark consult dirvish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
