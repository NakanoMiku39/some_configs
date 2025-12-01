(with-eval-after-load 'org
  ;; 设置标题的字体大小，逐级递减
  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.3 :weight 'semi-bold)
  (set-face-attribute 'org-level-4 nil :height 1.2 :weight 'semi-bold)
  (set-face-attribute 'org-level-5 nil :height 1.1)
  (set-face-attribute 'org-level-6 nil :height 1.0)
  (set-face-attribute 'org-level-7 nil :height 1.0)
  (set-face-attribute 'org-level-8 nil :height 1.0)
)

(provide 'org-mode)
