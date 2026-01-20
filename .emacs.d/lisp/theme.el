(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config))
(provide 'theme)
