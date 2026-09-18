(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; 启用真正的粗体字重
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; (load-theme 'doom-dracula t)
    (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(provide 'theme)
